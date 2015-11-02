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

import Prelude hiding (lookup)

import Control.Monad
import Control.Monad.Except

import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import qualified Language.Sill.AST as Ast
import Language.Sill.Parser.Location (Located (..), mergeLocated, SrcSpan)

import Language.Sill.TypeChecker.Common (Result, all_, any_)
import Language.Sill.TypeChecker.FreeVariables (freeChannels)
import Language.Sill.TypeChecker.Subtyping (subBase)
import qualified Language.Sill.TypeChecker.Types as Types

import Language.Sill.Utility.Error
import Language.Sill.Utility.Pretty (indentation)


type Base = Types.Base SrcSpan
type Property = Types.Property SrcSpan

type Exp = Ast.Exp SrcSpan
type Channel = Ast.Channel SrcSpan

type BaseContext = Map.Map Channel [Base]
type PropContext = [(Channel, Property)]
type Context = (BaseContext, PropContext)


{--------------------------------------------------------------------------
  External Interface
--------------------------------------------------------------------------}

checkFile :: Ast.File SrcSpan -> Either CompilerError ()
checkFile = runExcept . checkFile'

checkModule :: Ast.Module SrcSpan -> Either CompilerError ()
checkModule = runExcept . checkModule'


{--------------------------------------------------------------------------
  Internal Interface
--------------------------------------------------------------------------}

checkFile' :: Ast.File SrcSpan -> Result ()
checkFile' (Ast.File _ modules) = all_ (map checkModule' modules)

checkModule' :: Ast.Module SrcSpan -> Result ()
checkModule' (Ast.Module _ _ decls) = all_ (map checkDeclaration decls)

checkDeclaration :: Ast.Declaration SrcSpan -> Result ()
checkDeclaration (Ast.Declaration annot ident t p) = do
  let res = decomposeTarget (Map.empty, []) p [] [Types.into t]
  case runExcept res of
    Left err -> throwError $ makeError annot
      ("Cannot match the expected type " ++ show t)
      $ vcat [ text "in the definiton of" <+> pPrint ident <> colon
             , nest indentation (pPrint err)
             ]
    Right () -> return ()


{--------------------------------------------------------------------------
  Actual type-checker
--------------------------------------------------------------------------}

decomposeTarget :: Context -> Exp -> [Base] -> [Property] -> Result ()
decomposeTarget (bCtx, pCtx) e bases [] = decomposeContext bCtx pCtx e bases
decomposeTarget ctx e bases (p : ps) = case p of
  Types.TBase base -> decomposeTarget ctx e (base : bases) ps
  Types.TIntersect annot p1 p2 -> all_
    [ decomposeTarget ctx e bases (p1 : ps)
    , decomposeTarget ctx e bases (p2 : ps)
    ]
  Types.TUnion annot p1 p2 -> decomposeTarget ctx e bases (p1 : p2 : ps)


decomposeContext :: BaseContext -> PropContext -> Exp -> [Base] -> Result ()
decomposeContext bases [] e ts = checkBase bases e ts
decomposeContext bases ((d, p) : ps) e ts = case p of
  Types.TBase base -> decomposeContext (addType d base bases) ps e ts
  Types.TIntersect annot p1 p2 ->
    decomposeContext bases ((d, p1) : (d, p2) : ps) e ts
  Types.TUnion annot p1 p2 -> all_
    [ decomposeContext bases ((d, p1) : ps) e ts
    , decomposeContext bases ((d, p2) : ps) e ts
    ]


checkBase :: BaseContext -> Exp -> [Base] -> Result ()
-- id
checkBase ctx p@(Ast.EFwdProv _ d) targets = do
  dtypes <- lookup d ctx
  when (Map.size ctx > 1) $
    typeError (text "Unused channels in the context.") ctx p targets
  any_ [subBase dtype target | dtype <- dtypes, target <- targets]
-- cut
checkBase ctx p1@(Ast.ECut _ c p2 t p1') targets = do
  let t' = Types.into t
  let (ctx2, ctx1') = splitFree p2 ctx
  checkFree c ctx1'
  decomposeTarget (ctx2, []) p2 [] [t']
  decomposeContext ctx1' [(c, t')] p1' targets
-- 1R
checkBase ctx p@(Ast.ECloseProv _) targets = do
  unless (Map.null ctx) $
    typeError (text "Unused channels in the context.") ctx p targets
  when (null [() | t@Types.TUnit {} <- targets]) $
    typeError (text "Cannot close channel.") ctx p targets
  return ()
-- 1L
checkBase ctx p@(Ast.EWait _ d p') targets = do
  (dtypes, ctx') <- lookupRemove d ctx
  when (null [() | t@Types.TUnit {} <- dtypes]) $
    typeError (text "Cannot wait on non-closed channel") ctx p targets
  checkBase ctx' p' targets
-- productR
checkBase ctx p1@(Ast.ESendProv _ p2 p1') targets = do
  let products = [(a, b) | Types.TProduct _ a b <- targets]
  when (null products) $ typeError empty ctx p1 targets
  let (ctx2, ctx1') = splitFree p2 ctx
  let test (a, b) = decomposeTarget (ctx2, []) p2 [] [a]
                 >> decomposeTarget (ctx1', []) p1' [] [b]
  any_ $ map test products
-- productL
checkBase ctx p@(Ast.ERecv _ d c p') targets = do
  (ctypes, ctx') <- lookupRemove c ctx
  let products = [(a, b) | Types.TProduct _ a b <- ctypes]
  when (null products) $ typeError empty ctx p targets
  checkFree d ctx
  let test (a, b) = decomposeContext ctx [(d, a), (c, b)] p' targets
  any_ $ map test products
-- +R
checkBase ctx p@(Ast.ESelectProv _ lab p') targets = do
  let internals = [brs | Types.TInternal _ brs <- targets]
  let targets' = mapMaybe (Ast.branchLookup lab) internals
  when (null targets') $ typeError empty ctx p targets
  decomposeTarget (ctx, []) p' [] targets'
-- +L
checkBase ctx p@(Ast.ECase _ c cases) targets = do
  ctypes <- lookup c ctx
  let internals = [brs | Types.TInternal _ brs <- ctypes]
  when (null internals) $ typeError empty ctx p targets
  any_ $ map test internals
  where
    ctx' = Map.delete c ctx

    test :: [Ast.Branch Types.Property SrcSpan] -> Result ()
    test = checkBranches
      (\p t -> decomposeContext ctx' [(c, t)] p targets)
      (\s -> typeError s ctx p targets)
      cases
-- -o R
checkBase ctx p@(Ast.ERecvProv _ d p') targets = do
  let arrows = [(a, b) | Types.TArrow _ a b <- targets]
  when (null arrows) $ typeError empty ctx p targets
  checkFree d ctx
  let test (a, b) = decomposeTarget (ctx, [(d, a)]) p' [] [b]
  any_ $ map test arrows
-- -o L
checkBase ctx p1@(Ast.ESend _ c p2 p1') targets = do
  (ctypes, ctx') <- lookupRemove c ctx
  let arrows = [(a, b) | Types.TArrow _ a b <- ctypes]
  when (null arrows) $ typeError empty ctx p1 targets
  let (ctx2, ctx1') = splitFree p2 ctx'
  let test (a, b) = decomposeTarget (ctx2, []) p2 [] [a]
                 >> decomposeContext ctx1' [(c, b)] p1' targets
  any_ $ map test arrows
-- &R
checkBase ctx p@(Ast.ECaseProv _ cases) targets = do
  let externals = [brs | Types.TExternal _ brs <- targets]
  when (null externals) $ typeError empty ctx p targets
  any_ $ map test externals
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
  let ctypes' = mapMaybe (Ast.branchLookup lab) externals
  when (null ctypes') $ typeError empty ctx p targets
  decomposeContext ctx' (map (c,) ctypes') p' targets


-- | Ensure that all branches in the type have a matching case
checkBranches :: (Exp -> Property -> Result ())
              -> (Doc -> Result ())
              -> [Ast.Branch Ast.Exp SrcSpan]
              -> [Ast.Branch Types.Property SrcSpan]
              -> Result ()
checkBranches check error cases branches = all_ $ map checkBranch branches
  where
    cases' = Map.fromList $ map Ast.branchUnpack cases

    checkBranch :: Ast.Branch Types.Property SrcSpan -> Result ()
    checkBranch (Ast.Branch _ lab t) | Just p <- Map.lookup lab cases' =
      check p t
    checkBranch br = error (text "No case given for " <+> pPrint br)


{--------------------------------------------------------------------------
  Errors
--------------------------------------------------------------------------}

typeError :: Doc -> BaseContext -> Exp -> [Base] -> Result ()
typeError msg ctx e ts = throwError $ makeError (location e)
  ("Cannot match the expected type " ++ show t)
  (vcat [ msg
        , text "when checking expression"
        , pPrint e
        , text "in the context"
        , printContext ctx
        ])
  where t = foldr1 (Types.TUnion (location e)) (map Types.TBase ts)


printContext :: BaseContext -> Doc
printContext ctx = brackets $ vcat $ punctuate comma $
  map (uncurry typing) (Map.toList ctx)
  where
    typing :: Channel -> [Base] -> Doc
    typing c ts = pPrint c <+> colon <+> pPrint (intersect ts)

    intersect :: [Base] -> Property
    intersect ts = foldr1 (Types.TUnion (locations ts)) $ map Types.TBase ts

    locations :: [Base] -> SrcSpan
    locations = foldr1 mergeLocated . map location



-- TODO: Move this into its own file
{--------------------------------------------------------------------------
  Context management
--------------------------------------------------------------------------}

-- | Add a new type for the given channel
addType :: Channel -> Base -> BaseContext -> BaseContext
addType c t = Map.insertWith (++) c [t]

-- | Lookup the types for the given channel
lookup :: Channel -> BaseContext -> Result [Base]
lookup c ctx = case Map.lookup c ctx of
  Nothing -> throwError $ makeError (location c)
    ("Unbound channel " ++ show c ++ "in the context:")
    (printContext ctx)
  Just ts -> return ts

-- | Lookup the types for the given channel and remove them from the context
lookupRemove :: Channel -> BaseContext -> Result ([Base], BaseContext)
lookupRemove c ctx = do
  t <- lookup c ctx
  return (t, Map.delete c ctx)

-- | Ensure that the given channel does not occur in the context
checkFree :: Channel -> BaseContext -> Result ()
checkFree c ctx | Map.member c ctx = throwError $ makeError (location c)
  ("Channel binding shadows previously bound channel " ++ show c ++ "in the context:")
  (printContext ctx)
checkFree c ctx = return ()

-- | Split the context into two based on the free channels in the given process
splitFree :: Exp -> BaseContext -> (BaseContext, BaseContext)
splitFree p ctx = (ctx `Map.intersection` free, ctx Map.\\ free)
  where free = Map.fromList $ map (\c -> (c, ())) $ freeChannels p

