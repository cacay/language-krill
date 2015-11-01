{-# Language FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Elaborator
-- Description : Convert SILL syntax to simpler intermediate form
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- Elaboration combines expression lines into one expression, and puts all
-- parts of a process (type signature and all clauses) together. It also
-- checks for duplicate type signatures, duplicate or missing function
-- definitions.
-----------------------------------------------------------------------------
module Language.Sill.Elaborator
  ( elaborateFile
  , elaborateModule
  ) where

import Control.Arrow (first, second, (&&&))
import Control.Monad (foldM)
import Control.Monad.Except

import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.List (groupBy, sortOn)
import Data.Tuple (fst, snd)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import qualified Language.Sill.AST as Ast
import qualified Language.Sill.Parser.Syntax as Syn
import Language.Sill.Parser.Annotated
import Language.Sill.Parser.Location

import Language.Sill.Utility.Error
import Language.Sill.Utility.Pretty


type Result = Except CompilerError

type Context = Map.Map String (Loc (Ast.Type SrcSpan))


{--------------------------------------------------------------------------
  External interface
--------------------------------------------------------------------------}

-- | Elaborate a file (external interface)
elaborateFile :: Syn.File SrcSpan -> Either CompilerError (Ast.File SrcSpan)
elaborateFile = runExcept . elabFile

-- | Elaborate a module (external interface)
elaborateModule :: Syn.Module SrcSpan -> Either CompilerError (Ast.Module SrcSpan)
elaborateModule = runExcept . elabModule


{--------------------------------------------------------------------------
  Local data types
--------------------------------------------------------------------------}

{- We process type signatures and function clauses separately. We use the following
   data types to do so.
-}

data TypeSig annot = TypeSig annot (Syn.Ident annot) (Syn.Type annot)

data FunClause annot =
  FunClause annot (Syn.Channel annot) (Syn.Ident annot) (Syn.Exp annot)


sigId :: TypeSig SrcSpan -> String
sigId (TypeSig _ (Syn.Ident _ id) _) = id

clauseId :: FunClause SrcSpan -> String
clauseId (FunClause _ _ (Syn.Ident _ id) _) = id


instance Annotated TypeSig where
  annot (TypeSig annot _ _) = annot

instance Annotated FunClause where
  annot (FunClause annot _ _ _) = annot

instance Pretty (TypeSig SrcSpan) where
  pPrint (TypeSig annot ident t) =
    pPrint t <+> parens (text "declared at" <+> pPrint annot)

instance Pretty (FunClause SrcSpan) where
  pPrint (FunClause annot c ident exp) =
    text "Defined at" <+> pPrint annot <> colon
    $$ pPrint (Syn.FunClause annot c ident exp)


{--------------------------------------------------------------------------
  Internal functions
--------------------------------------------------------------------------}

-- | Elaborate a file (internal)
elabFile :: Syn.File SrcSpan -> Result (Ast.File SrcSpan)
elabFile (Syn.File annot ms) = liftM (Ast.File annot) (mapM elabModule ms)

-- | Elaborate a module (external)
elabModule :: Syn.Module SrcSpan -> Result (Ast.Module SrcSpan)
elabModule (Syn.Module annot name decls) = do
  decls' <- elabDeclarations decls
  return $ Ast.Module annot (elabIdent name) decls'


elabDeclarations :: [Syn.Declaration SrcSpan] -> Result [Ast.Declaration SrcSpan]
elabDeclarations decls = do
  ctx <- elabTypeSigs sigs
  let notDefined = Map.toList $ ctx Map.\\ defined
  let notDeclared = Map.toList $ defined Map.\\ ctx
  unless (null notDefined) $ elabErrors (map missingDefinition notDefined)
  unless (null notDeclared) $ elabErrors (map missingSignature notDeclared)
  elabFunClauses ctx clauses
  where
    sigs = [TypeSig annot id t | Syn.TypeSig annot id t <- decls]
    clauses = [FunClause annot c id e | Syn.FunClause annot c id e <- decls]

    -- All defined function names
    defined = Map.fromList $ map (clauseId &&& location) clauses

    missingDefinition :: (String, Loc (Ast.Type SrcSpan)) -> CompilerError
    missingDefinition (id, loc) = makeError (location loc)
      ("No clauses given for " ++ id) empty

    missingSignature :: (String, SrcSpan) -> CompilerError
    missingSignature (id, loc) = makeError (location loc)
      ("No type signature given for " ++ id) empty

-- | Elaborate type signatures while checking for duplicates
elabTypeSigs :: [TypeSig SrcSpan] -> Result Context
elabTypeSigs sigs | sigs <- sortOn sigId sigs = do
  checkDuplicatesOn sigId (\id -> "Multiple type signatures given for " ++ id) sigs
  return $ Map.fromAscList $ map elab sigs
  where
    elab :: TypeSig SrcSpan -> (String, Loc (Ast.Type SrcSpan))
    elab sig@(TypeSig annot _ t) = (sigId sig, makeLoc annot $ elabType t)

-- | Elaborate clauses. Attaches type signatures, and checks for
-- duplicate definitions. The context _must_ contain type signatures for
-- all defined functions.
elabFunClauses :: Context -> [FunClause SrcSpan] -> Result [Ast.Declaration SrcSpan]
elabFunClauses ctx clauses | clauses <- sortOn clauseId clauses = do
  checkDuplicatesOn clauseId (\id -> "Multiple definitions given for " ++ id) clauses
  mapM elab clauses
  where
    elab :: FunClause SrcSpan -> Result (Ast.Declaration SrcSpan)
    elab clause@(FunClause annot c ident e) = do
      let t = ctx Map.! (clauseId clause)
      e' <- elabExp e
      return $ Ast.Declaration annot (elabIdent ident) (elabChannel c) (unLoc t) e'


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


-- | Assert that all elements of a _sorted_ list are distinct. Uses the given
-- function to extract the key to compare on.
checkDuplicatesOn :: forall key e . (Eq key, Show key, Located e, Pretty e)
                  => (e -> key)       -- ^ Extract a key to compare on
                  -> (key -> String)  -- ^ The message to display for duplicates
                  -> [e]              -- ^ List of inputs
                  -> Result ()
checkDuplicatesOn key msg es =
    unless (null duplicates) $ elabErrors $ map duplicateError duplicates
  where
    -- Find duplicates (requires 'es' to be sorted on keys)
    duplicates :: [[e]]
    duplicates = filter (\l -> length l >= 2) $ groupBy ((==) `on` key) es

    -- 'CompileError' for a duplicate definition
    duplicateError :: [e] -> CompilerError
    duplicateError (ds@(d : _)) = makeError (location d)
      (msg (key d) ++ ":")
      (nest indentation $ vcat $ map pPrint ds)


-- | Throw an error
elabError :: SrcSpan -> String -> Doc -> Result a
elabError loc msg details = throwError (makeError loc msg details)

-- Throw a non-empty list of errors
elabErrors :: [CompilerError] -> Result a
elabErrors = throwError . foldl1 combineErrors

