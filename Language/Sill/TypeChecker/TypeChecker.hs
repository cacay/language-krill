{-# Language TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.TypeChecker
-- Description : Type-checker for the SILL language
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.TypeChecker
  ( checkFile
  , checkModule
  )
  where

import Control.Monad
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Sill.Monad.Compiler
import Language.Sill.Parser.Location (Located (..), mergeLocated, SrcSpan)

import qualified Language.Sill.AST as Ast

import Language.Sill.TypeChecker.Context (Context, typedefs, functions, channels)
import qualified Language.Sill.TypeChecker.Context as Context
import Language.Sill.TypeChecker.FreeVariables (freeChannels)
import Language.Sill.TypeChecker.Subtyping (subBase)
import qualified Language.Sill.TypeChecker.Types as Types

import Language.Sill.Utility.Pretty (indentation, period)


----------------------------------------------------------------------------
-- * Shorthands
----------------------------------------------------------------------------

type Base = Types.Base SrcSpan
type Property = Types.Property SrcSpan

type Exp = Ast.Exp SrcSpan

type Ident = Ast.Ident SrcSpan
type Constructer = Ast.Constructer SrcSpan
type Channel = Ast.Channel SrcSpan


----------------------------------------------------------------------------
-- * Context
----------------------------------------------------------------------------

type PropContext = [(Channel, Property)]


{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}

checkFile :: Ast.File SrcSpan -> Compiler ()
checkFile (Ast.File _ modules) = runAll_ (map checkModule modules)

checkModule :: Ast.Module SrcSpan -> Compiler ()
checkModule (Ast.Module _ _ decls) =
  runAll_ (map (checkDeclaration Context.empty) decls)


checkDeclaration :: Context -> Ast.Declaration SrcSpan -> Compiler ()
checkDeclaration ctx (Ast.Declaration annot ident t p) = do
  let m = decomposeTarget (ctx, []) p [] [Types.into t]
  m `inContext` makeCompilerContext (Just annot)
    (text "Type errors in the definition of" <+> pPrint ident <> colon)
    (text "Cannot match the expected type" <+> pPrint t)


{--------------------------------------------------------------------------
  Actual type-checker
--------------------------------------------------------------------------}

decomposeTarget :: (Context, PropContext) -> Exp -> [Base] -> [Property] -> Compiler ()
decomposeTarget (ctx, pCtx) e bases [] = decomposeContext ctx pCtx e bases
decomposeTarget ctx e bases (p : ps) = case p of
  Types.TBase base -> decomposeTarget ctx e (base : bases) ps
  Types.TIntersect annot p1 p2 -> runAll_
    [ decomposeTarget ctx e bases (p1 : ps)
    , decomposeTarget ctx e bases (p2 : ps)
    ]
  Types.TUnion annot p1 p2 -> decomposeTarget ctx e bases (p1 : p2 : ps)


decomposeContext :: Context -> PropContext -> Exp -> [Base] -> Compiler ()
decomposeContext ctx [] e ts = checkBase ctx e ts
decomposeContext ctx ((d, p) : ps) e ts = case p of
  Types.TBase base -> decomposeContext (Context.add channels d base ctx) ps e ts
  Types.TIntersect annot p1 p2 ->
    decomposeContext ctx ((d, p1) : (d, p2) : ps) e ts
  Types.TUnion annot p1 p2 -> runAll_
    [ decomposeContext ctx ((d, p1) : ps) e ts
    , decomposeContext ctx ((d, p2) : ps) e ts
    ]


checkBase :: Context -> Exp -> [Base] -> Compiler ()
-- id
checkBase ctx p@(Ast.EFwdProv _ d) targets = do
  (dtypes, ctx') <- lookupRemove d ctx
  unless (Context.null channels ctx') $
    typeError (text "Unused channels in the context.") ctx p targets
  runAny_ [subBase dtype target | dtype <- dtypes, target <- targets]
-- cut
checkBase ctx p1@(Ast.ECut _ c p2 t p1') targets = do
  let t' = Types.into t
  let (ctx2, ctx1') = splitFree p2 ctx
  checkFree c ctx1'
  decomposeTarget (ctx2, []) p2 [] [t']
  decomposeContext ctx1' [(c, t')] p1' targets
-- 1R
checkBase ctx p@(Ast.ECloseProv _) targets = do
  unless (Context.null channels ctx) $
    typeError (text "Unused channels in the context.") ctx p targets
  when (null [() | t@Types.TUnit {} <- targets]) $
    typeError (text "Cannot close a channel that does not have type 1.")
      ctx p targets
  return ()
-- 1L
checkBase ctx p@(Ast.EWait _ d p') targets = do
  (dtypes, ctx') <- lookupRemove d ctx
  when (null [() | t@Types.TUnit {} <- dtypes]) $
    matchError (Just d) (text "1") dtypes ctx p targets
  checkBase ctx' p' targets
-- productR
checkBase ctx p1@(Ast.ESendProv _ p2 p1') targets = do
  let products = [(a, b) | Types.TProduct _ a b <- targets]
  when (null products) $
    matchError Nothing (text "_ * _") targets ctx p1 targets
  let (ctx2, ctx1') = splitFree p2 ctx
  let test (a, b) = decomposeTarget (ctx2, []) p2 [] [a]
                 >> decomposeTarget (ctx1', []) p1' [] [b]
  runAny_ $ map test products
-- productL
checkBase ctx p@(Ast.ERecv _ d c p') targets = do
  (ctypes, ctx') <- lookupRemove c ctx
  let products = [(a, b) | Types.TProduct _ a b <- ctypes]
  when (null products) $
    matchError (Just c) (text "_ * _") ctypes ctx p targets
  checkFree d ctx
  let test (a, b) = decomposeContext ctx [(d, a), (c, b)] p' targets
  runAny_ $ map test products
-- +R
checkBase ctx p@(Ast.ESelectProv _ lab p') targets = do
  let internals = [brs | Types.TInternal _ brs <- targets]
  when (null internals) $
    matchError Nothing (text "+{...}") targets ctx p targets
  let targets' = mapMaybe (Ast.branchLookup lab) internals
  when (null targets') $
    typeError (text "Invalid selection" <+> quotes (pPrint lab)) ctx p targets
  decomposeTarget (ctx, []) p' [] targets'
-- +L
checkBase ctx p@(Ast.ECase _ c cases) targets = do
  ctypes <- lookupChannel c ctx
  let internals = [brs | Types.TInternal _ brs <- ctypes]
  when (null internals) $
    matchError (Just c) (text "+{...}") ctypes ctx p targets
  runAny_ $ map test internals
  where
    ctx' = Context.delete channels c ctx

    test :: [Ast.Branch Types.Property SrcSpan] -> Compiler ()
    test = checkBranches
      (\p t -> decomposeContext ctx' [(c, t)] p targets)
      (\s -> typeError s ctx p targets)
      cases
-- -o R
checkBase ctx p@(Ast.ERecvProv _ d p') targets = do
  let arrows = [(a, b) | Types.TArrow _ a b <- targets]
  when (null arrows) $
    matchError Nothing (text "_ -o _") targets ctx p targets
  checkFree d ctx
  let test (a, b) = decomposeTarget (ctx, [(d, a)]) p' [] [b]
  runAny_ $ map test arrows
-- -o L
checkBase ctx p1@(Ast.ESend _ c p2 p1') targets = do
  (ctypes, ctx') <- lookupRemove c ctx
  let arrows = [(a, b) | Types.TArrow _ a b <- ctypes]
  when (null arrows) $
    matchError (Just c) (text "_ -o _") ctypes ctx p1 targets
  let (ctx2, ctx1') = splitFree p2 ctx'
  let test (a, b) = decomposeTarget (ctx2, []) p2 [] [a]
                 >> decomposeContext ctx1' [(c, b)] p1' targets
  runAny_ $ map test arrows
-- &R
checkBase ctx p@(Ast.ECaseProv _ cases) targets = do
  let externals = [brs | Types.TExternal _ brs <- targets]
  when (null externals) $
    matchError Nothing (text "&{...}") targets ctx p targets
  runAny_ $ map test externals
  where
    test :: [Ast.Branch Types.Property SrcSpan] -> Compiler ()
    test = checkBranches
      (\p t -> decomposeTarget (ctx, []) p [] [t])
      (\s -> typeError s ctx p targets)
      cases
-- &L
checkBase ctx p@(Ast.ESelect _ c lab p') targets = do
  (ctypes, ctx') <- lookupRemove c ctx
  let externals = [brs | Types.TExternal _ brs <- ctypes]
  when (null externals) $
    matchError (Just c) (text "&{...}") ctypes ctx p targets
  let ctypes' = mapMaybe (Ast.branchLookup lab) externals
  when (null ctypes') $
    typeError (text "Invalid selection" <+> quotes (pPrint lab)) ctx p targets
  decomposeContext ctx' (map (c,) ctypes') p' targets


-- | Ensure that all branches in the type have a matching case
checkBranches :: (Exp -> Property -> Compiler ())
              -> (Doc -> Compiler ())
              -> [Ast.Branch Ast.Exp SrcSpan]
              -> [Ast.Branch Types.Property SrcSpan]
              -> Compiler ()
checkBranches check error cases branches = runAll_ $ map checkBranch branches
  where
    cases' = Map.fromList $ map Ast.branchUnpack cases

    checkBranch :: Ast.Branch Types.Property SrcSpan -> Compiler ()
    checkBranch (Ast.Branch _ lab t) | Just p <- Map.lookup lab cases' =
      check p t
    checkBranch br = error (text "No case given for" <+> braces (pPrint br))


{--------------------------------------------------------------------------
  Errors
--------------------------------------------------------------------------}

typeError :: Doc -> Context -> Exp -> [Base] -> Compiler ()
typeError msg ctx e ts = compilerError (location e) $
  vcat [ text "Cannot match the expected type" <+> pPrint t <> colon
       , if isEmpty msg then empty else text "Error:" <+> msg
       , text "While checking the expression"
       , nest indentation $ pPrint e
       , text "in the context"
       , nest indentation $ Context.prettyChannels ctx
       ]
  where t = foldr1 (Types.TUnion $ location e) (map Types.TBase ts)

matchError :: Maybe Channel
           -> Doc
           -> [Base]
           -> Context
           -> Exp
           -> [Base]
           -> Compiler ()
matchError c expected got ctx e ts = compilerError (location e)
  (vcat [ text "Cannot match the expected structure:"
            <+> channel <+> colon <+> expected <> period
        , text "Available options were:"
        , nest indentation (vcat $ map pPrint got)
        ])
  `inContext` makeCompilerContext Nothing empty
    (vcat [ text "While checking the expression"
         , nest indentation $ pPrint e
         , text "in the context"
         , nest indentation $ Context.prettyChannels ctx
         ])
  where t = foldr1 (Types.TUnion $ location e) (map Types.TBase ts)
        channel = case c of {Nothing -> text "_"; Just c -> pPrint c}



{--------------------------------------------------------------------------
  Context management
--------------------------------------------------------------------------}

-- | Lookup the types for the given channel
lookupChannel :: Channel -> Context -> Compiler [Base]
lookupChannel c ctx = case Context.lookup channels c ctx of
  Nothing -> compilerError (location c) $
    text "Unbound channel" <+> pPrint c <+> text "in the context" <> colon
      $$ Context.prettyChannels ctx
  Just ts -> return ts

-- | Lookup the types for the given channel and remove them from the context
lookupRemove :: Channel -> Context -> Compiler ([Base], Context)
lookupRemove c ctx = do
  t <- lookupChannel c ctx
  return (t, Context.delete channels c ctx)

-- | Ensure that the given channel does not occur in the context
checkFree :: Channel -> Context -> Compiler ()
checkFree c ctx | Context.member channels c ctx = compilerError (location c) $
  text "Channel binding shadows previously bound channel" <+> pPrint c
    <+> text "in the context:" $$ Context.prettyChannels ctx
checkFree c ctx = return ()

-- | Split the context into two based on the free channels in the given process
splitFree :: Exp -> Context -> (Context, Context)
splitFree p = Context.split channels (freeChannels p)

