{-# Language TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Krill.TypeChecker.TypeChecker
-- Description : Type-checker for the Krill language
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Krill.TypeChecker.TypeChecker
  ( checkFile
  , checkModule
  )
  where

import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Reader

import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Krill.Monad.Compiler
import Language.Krill.Parser.Location (Located (..), mergeLocated, SrcSpan)

import qualified Language.Krill.AST as Ast

import Language.Krill.TypeChecker.Context (Context, typedefs, functions, channels)
import qualified Language.Krill.TypeChecker.Context as Context
import Language.Krill.TypeChecker.Contractivity (contractiveModule)
import Language.Krill.TypeChecker.FreeVariables (freeChannels)
import Language.Krill.TypeChecker.Subtyping (subBase, TypeDefs)
import qualified Language.Krill.TypeChecker.Types as Types

import Language.Krill.Utility.Pretty (indentation, period)


----------------------------------------------------------------------------
-- * Shorthands
----------------------------------------------------------------------------

type Base = Types.Base SrcSpan
type Property = Types.Property SrcSpan

type Exp = Ast.Exp SrcSpan

type Ident = Ast.Ident SrcSpan
type Constructor = Ast.Constructor SrcSpan
type Channel = Ast.Channel SrcSpan


----------------------------------------------------------------------------
-- * Context
----------------------------------------------------------------------------

type TypeContext = ( Map.Map (Ast.Constructor SrcSpan) Property
                   , Map.Map (Ast.Ident SrcSpan) Property
                   )
type PropContext = [(Channel, Property)]

type Result = CompilerT (Reader TypeContext)


{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}

checkFile :: Ast.File SrcSpan -> Compiler ()
checkFile (Ast.File _ modules) = runAll_ (map checkModule modules)

checkModule :: Ast.Module SrcSpan -> Compiler ()
checkModule mod@(Ast.Module _ _ typedefs funcs) = do
  contractiveModule mod
  run (Map.fromList $ map getTypeDef typedefs, Map.fromList $ map getFunSig funcs)
    $ runAll_ (map (checkFunction Context.empty) funcs)
  where
    getTypeDef :: Ast.TypeDef SrcSpan -> (Ast.Constructor SrcSpan, Property)
    getTypeDef (Ast.TypeDef _ con t) = (con, Types.into t)

    getFunSig :: Ast.Function SrcSpan -> (Ast.Ident SrcSpan, Property)
    getFunSig (Ast.Function _ id t _) = (id, Types.into t)


-- | Unfold the 'Reader' monad to get a 'Compiler' monad
run :: TypeContext -> Result a -> Compiler a
run ctx m =
  case runReader (runCompilerT m) ctx of
    Left e -> throwError e
    Right a -> return a


checkFunction :: Context -> Ast.Function SrcSpan -> Result ()
checkFunction ctx (Ast.Function annot ident t p) = do
  let m = decomposeTarget (ctx, []) p [] [Types.into t]
  m `inContext` makeCompilerContext (Just annot)
    (text "Type errors in the definition of" <+> pPrint ident <> colon)
    (text "Cannot match the expected type" <+> pPrint t)


{--------------------------------------------------------------------------
  Actual type-checker
--------------------------------------------------------------------------}

decomposeTarget :: (Context, PropContext) -> Exp -> [Base] -> [Property] -> Result ()
decomposeTarget (ctx, pCtx) e bases [] = decomposeContext ctx pCtx e bases
decomposeTarget ctx e bases (p : ps) = case p of
  Types.TBase base -> decomposeTarget ctx e (base : bases) ps
  Types.TIntersect annot p1 p2 -> runAll_
    [ decomposeTarget ctx e bases (p1 : ps)
    , decomposeTarget ctx e bases (p2 : ps)
    ]
  Types.TUnion annot p1 p2 -> decomposeTarget ctx e bases (p1 : p2 : ps)
  Types.TVar annot p -> do
    p' <- unfold p
    decomposeTarget ctx e bases (p' : ps)


decomposeContext :: Context -> PropContext -> Exp -> [Base] -> Result ()
decomposeContext ctx [] e ts = checkBase ctx e ts
decomposeContext ctx ((d, p) : ps) e ts = case p of
  Types.TBase base -> decomposeContext (Context.add channels d base ctx) ps e ts
  Types.TIntersect annot p1 p2 ->
    decomposeContext ctx ((d, p1) : (d, p2) : ps) e ts
  Types.TUnion annot p1 p2 -> runAll_
    [ decomposeContext ctx ((d, p1) : ps) e ts
    , decomposeContext ctx ((d, p2) : ps) e ts
    ]
  Types.TVar annot p -> do
    p' <- unfold p
    decomposeContext ctx ((d, p') : ps) e ts


checkBase :: Context -> Exp -> [Base] -> Result ()
-- id
checkBase ctx p@(Ast.EFwdProv _ d) targets = do
  (dtypes, ctx') <- lookupRemove d ctx
  unless (Context.null channels ctx') $
    typeError (text "Unused channels in the context.") ctx' p targets
  typedefs <- asks fst
  runAny_ [subBaseL typedefs dtype target | dtype <- dtypes, target <- targets]
    `inContext` makeCompilerContext (Just $ location p) empty
    (text "When checking" <+> pPrint p)
  where
    subBaseL :: TypeDefs -> Base -> Base -> Result ()
    subBaseL typedefs a b =
      case runCompiler (subBase typedefs a b) of
        Left e -> throwError e
        Right () -> return ()
-- cut
checkBase ctx p@(Ast.ECut _ c ident p') targets = do
  checkFree c ctx
  t <- lookupTypeSig ident
  decomposeContext ctx [(c, t)] p' targets
-- 1R
checkBase ctx p@(Ast.ECloseProv _) targets = do
  unless (Context.null channels ctx) $
    typeError (text "Unused channels in the context.") ctx p targets
  when (null [() | Types.TUnit {} <- targets]) $
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

    test :: [Ast.Branch Types.Property SrcSpan] -> Result ()
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
    test :: [Ast.Branch Types.Property SrcSpan] -> Result ()
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
checkBranches :: (Exp -> Property -> Result ())
              -> (Doc -> Result ())
              -> [Ast.Branch Ast.Exp SrcSpan]
              -> [Ast.Branch Types.Property SrcSpan]
              -> Result ()
checkBranches check error cases branches = runAll_ $ map checkBranch branches
  where
    cases' = Map.fromList $ map Ast.branchUnpack cases

    checkBranch :: Ast.Branch Types.Property SrcSpan -> Result ()
    checkBranch (Ast.Branch _ lab t) | Just p <- Map.lookup lab cases' =
      check p t
    checkBranch br = error (text "No case given for" <+> braces (pPrint br))


{--------------------------------------------------------------------------
  Errors
--------------------------------------------------------------------------}

typeError :: Doc -> Context -> Exp -> [Base] -> Result ()
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
           -> Result ()
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

-- | Unfold a type definition
unfold :: Ast.Constructor SrcSpan -> Result Property
unfold con = do
  def <- asks (Map.lookup con . fst)
  case def of
    Nothing -> compilerError (location con) $
      text "Undefined constructor:" <+> pPrint con
    Just prop -> return prop

-- | Lookup the type signature for a function
lookupTypeSig :: Ast.Ident SrcSpan -> Result Property
lookupTypeSig ident = do
  def <- asks (Map.lookup ident . snd)
  case def of
    Nothing -> compilerError (location ident) $
      text "Undefined function:" <+> pPrint ident
    Just prop -> return prop


-- | Lookup the types for the given channel
lookupChannel :: Channel -> Context -> Result [Base]
lookupChannel c ctx = case Context.lookup channels c ctx of
  Nothing -> compilerError (location c) $
    text "Unbound channel" <+> pPrint c <+> text "in the context" <> colon
      $$ Context.prettyChannels ctx
  Just ts -> return ts

-- | Lookup the types for the given channel and remove them from the context
lookupRemove :: Channel -> Context -> Result ([Base], Context)
lookupRemove c ctx = do
  t <- lookupChannel c ctx
  return (t, Context.delete channels c ctx)

-- | Ensure that the given channel does not occur in the context
checkFree :: Channel -> Context -> Result ()
checkFree c ctx | Context.member channels c ctx = compilerError (location c) $
  text "Channel binding shadows previously bound channel" <+> pPrint c
    <+> text "in the context:" $$ Context.prettyChannels ctx
checkFree c ctx = return ()

-- | Split the context into two based on the free channels in the given process
splitFree :: Exp -> Context -> (Context, Context)
splitFree p = Context.split channels (freeChannels p)

