-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Elaborator
-- Description : Convert SILL syntax to simpler intermediate form
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Elaborator
  ( elaborateFile
  , elaborateModule
  ) where

import Control.Arrow (first, second)
import Control.Monad (foldM)
import Control.Monad.Except

import Data.Function (on)
import qualified Data.Map as Map
import Data.List (sortOn)
import Data.Tuple (fst, snd)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Sill.AST as Ast
import Language.Sill.Parser.Syntax as Syn
import Language.Sill.Parser.Location

import Language.Sill.Utility.Error
import Language.Sill.Utility.List (group2)
import Language.Sill.Utility.Pretty


type Result = Except CompilerError

type Context = Map.Map String (Ast.Type SrcSpan)


-- | Elaborate a file (external interface)
elaborateFile :: Syn.File SrcSpan -> Either CompilerError (Ast.File SrcSpan)
elaborateFile = runExcept . elabFile

-- | Elaborate a module (external interface)
elaborateModule :: Syn.Module SrcSpan -> Either CompilerError (Ast.Module SrcSpan)
elaborateModule = runExcept . elabModule

-- | Elaborate a file
elabFile :: Syn.File SrcSpan -> Result (Ast.File SrcSpan)
elabFile (Syn.File annot ms) = liftM (Ast.File annot) (mapM elabModule ms)

-- | Elaborate a module
-- TODO: Check for declared by undefined functions
elabModule :: Syn.Module SrcSpan -> Result (Ast.Module SrcSpan)
elabModule (Syn.Module annot name decls) = do
  unless (null duplicates) $
    throwError (foldl1 combineErrors $ map duplicateError duplicates)
  decls' <- mapM (elabClause context) [c | c@(Syn.FunClause {}) <- decls]
  return $ Ast.Module annot (elabIdent name) decls'
  where
    -- All type signatures ordered by the identifier
    sigs :: [(String, Loc (Ast.Type SrcSpan))]
    sigs = sortOn fst [(id, makeLoc annot (elabType t))
      | Syn.TypeSig annot (Syn.Ident _ id) t <- decls]

    -- Context containing type-signatures for identifiers (requires sigs to be sorted)
    context :: Context
    context = Map.fromAscList $ map (second unLoc) sigs

    -- Find duplicate signatures (requires sigs to be sorted on identifiers)
    duplicates :: [(String, [Loc (Ast.Type SrcSpan)])]
    duplicates = filter (\l -> length (snd l) >= 2) (group2 sigs)

    -- 'CompileError' for a duplicate definition
    duplicateError :: (String, [Loc (Ast.Type SrcSpan)]) -> CompilerError
    duplicateError (ident, decls@(d : ds)) = makeError (location d)
      ("Multiple type signatures for " ++ ident ++ ":")
      (nest indentation $ vcat $ map pPrint decls)

-- | Elaborate a function clause
elabClause :: Context
           -> Syn.Declaration SrcSpan
           -> Result (Ast.Declaration SrcSpan)
elabClause ctx (Syn.FunClause annot c ident@(Syn.Ident _ name) e) =
  case Map.lookup name ctx of
    Nothing -> elabError annot "Missing type signature for expression" (pPrint e)
    Just t -> do
      e' <- elabExp e
      return $ Ast.Declaration annot (elabIdent ident) (elabChannel c) t e'


-- | Elaborate a type
elabType :: Syn.Type annot -> Ast.Type annot
elabType (Syn.TUnit annot) = Ast.TUnit annot
elabType (Syn.TProduct annot a b) = Ast.TProduct annot (elabType a) (elabType b)
elabType (Syn.TArrow annot a b) = Ast.TArrow annot (elabType a) (elabType b)
elabType (Syn.TInternal annot br) =
  Ast.TInternal annot $ map (elabBranch elabType) br
elabType (Syn.TExternal annot br) =
  Ast.TExternal annot $ map (elabBranch elabType) br
elabType (Syn.TIntersect annot a b) =
  Ast.TIntersect annot (elabType a) (elabType b)
elabType (Syn.TUnion annot a b) = Ast.TUnion annot (elabType a) (elabType b)


-- | Elaborate an expression. Combines multiple 'Syn.ExpLine's into one 'Ast.Exp'.
elabExp :: Syn.Exp SrcSpan -> Result (Ast.Exp SrcSpan)
elabExp e@(Syn.Exp annot []) =
  elabError (location e) "Process expression cannot be empty" (pPrint e)
elabExp (Syn.Exp annot es) | r : rs <- reverse es = do
  r' <- elabLast r
  foldM (flip elabLine) r' rs
  where
    -- Convert a terminating expression
    elabLast :: Syn.ExpLine SrcSpan -> Result (Ast.Exp SrcSpan)
    elabLast (Syn.EFwd annot c d) =
      return $ Ast.EFwd annot (elabChannel c) (elabChannel d)
    elabLast (Syn.EClose annot c) = return $ Ast.EClose annot (elabChannel c)
    elabLast (Syn.ECase annot c br) = do
      br' <- mapM branch br
      return $ Ast.ECase annot (elabChannel c) br'
      where
        branch :: Syn.Branch Syn.Exp SrcSpan -> Result (Ast.Branch Ast.Exp SrcSpan)
        branch (Syn.Branch annot lab e) = do
          e' <- elabExp e
          return $ Ast.Branch annot (elabLabel lab) e'
    elabLast e = elabError (location e)
      "The following cannot be the last expression of a process:"
      (pPrint e)

    -- Convert a non-terminating expression
    elabLine :: Syn.ExpLine SrcSpan -> Ast.Exp SrcSpan -> Result (Ast.Exp SrcSpan)
    elabLine (Syn.ECut annot c e1 t) e2 = do
      e1' <- elabExp e1
      return $ Ast.ECut (mergeLocated annot e2) (elabChannel c) e1' (elabType t) e2
    elabLine (Syn.EWait annot c) e =
      return $ Ast.EWait (mergeLocated annot e) (elabChannel c) e
    elabLine (Syn.ESend annot c d) e =
      return $ Ast.ESend (mergeLocated annot e) (elabChannel c) (elabChannel d) e
    elabLine (Syn.ERecv annot c d) e =
      return $ Ast.ERecv (mergeLocated annot e) (elabChannel c) (elabChannel d) e
    elabLine (Syn.ESelect annot c lab) e =
      return $ Ast.ESelect (mergeLocated annot e) (elabChannel c) (elabLabel lab) e
    elabLine e1 e2 = elabError (location e1)
      "Unexpected expressions after terminating expression:"
      (pPrint e1)


elabIdent :: Syn.Ident annot -> Ast.Ident annot
elabIdent (Syn.Ident annot ident) = Ast.Ident annot ident

elabChannel :: Syn.Channel annot -> Ast.Channel annot
elabChannel (Syn.Channel annot c) = Ast.Channel annot c

elabLabel :: Syn.Label annot -> Ast.Label annot
elabLabel (Syn.Label annot lab) = Ast.Label annot lab

elabBranch :: (t1 annot -> t2 annot) -> Syn.Branch t1 annot -> Ast.Branch t2 annot
elabBranch elab (Syn.Branch annot lab t) =
  Ast.Branch annot (elabLabel lab) (elab t)


elabError :: SrcSpan -> String -> Doc -> Result a
elabError loc msg details = throwError (makeError loc msg details)

