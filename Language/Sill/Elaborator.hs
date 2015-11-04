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

-- TODO: Check for duplicate branches in types

import Control.Arrow ((&&&))
import Control.Monad

import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (groupBy, sortOn)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import Language.Sill.Monad.Compiler
import qualified Language.Sill.AST as Ast
import qualified Language.Sill.Parser.Syntax as Syn
import Language.Sill.Parser.Annotated
import Language.Sill.Parser.Location

import Language.Sill.Utility.Pretty


type Context = Map.Map String (Loc (Ast.Type SrcSpan))


{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}

-- | Elaborate a file
elaborateFile :: Syn.File SrcSpan -> Compiler (Ast.File SrcSpan)
elaborateFile (Syn.File annot ms) =
  liftM (Ast.File annot) (mapM elaborateModule ms)

-- | Elaborate a module (external)
elaborateModule :: Syn.Module SrcSpan -> Compiler (Ast.Module SrcSpan)
elaborateModule (Syn.Module annot name decls) = do
  decls' <- elabDeclarations decls
  return $ Ast.Module annot (elabIdent name) decls'


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

elabDeclarations :: [Syn.Declaration SrcSpan] -> Compiler [Ast.Declaration SrcSpan]
elabDeclarations decls = do
  runAll_
    [ runAll_ $ map checkDefined sigs
    , runAll_ $ map checkSignature clauses
    , checkDuplicatesOn sigId
        (\id -> text "Multiple type signatures given for" <+> text id)
        sigs
    , checkDuplicatesOn clauseId
        (\id -> text "Multiple clauses given for" <+> text id)
        clauses
    ]
  ctx <- elabTypeSigs sigs
  elabFunClauses ctx clauses
  where
    sigs = [TypeSig annot id t | Syn.TypeSig annot id t <- decls]
    clauses = [FunClause annot c id e | Syn.FunClause annot c id e <- decls]

    -- Set of defined function names
    clauseIdSet = Set.fromList $ map clauseId clauses
    -- Set of declared type signatures
    sigIdSet = Set.fromList $ map sigId sigs

    -- Check that the type signature has matching clauses
    checkDefined :: TypeSig SrcSpan -> Compiler ()
    checkDefined sig@(TypeSig annot ident _) =
      unless (Set.member (sigId sig) clauseIdSet) $ compilerError annot
        (text "No clauses given for" <+> pPrint ident <> period)

    -- Check that the clause has a matching type signature
    checkSignature :: FunClause SrcSpan -> Compiler ()
    checkSignature clause@(FunClause annot _ ident _) =
      unless (Set.member (clauseId clause) sigIdSet) $ compilerError annot
        (text "No type signature given for" <+> pPrint ident <> period)


-- | Elaborate type signatures.
elabTypeSigs :: [TypeSig SrcSpan] -> Compiler Context
elabTypeSigs = liftM Map.fromList . runAll . map elabTypeSig
  where
    elabTypeSig :: TypeSig SrcSpan -> Compiler (String, Loc (Ast.Type SrcSpan))
    elabTypeSig sig@(TypeSig annot ident t) = do
     t' <- elabType t `inContext` makeCompilerContext (Just annot)
             empty (text "In the signature for" <+> pPrint ident)
     return (sigId sig, makeLoc annot t')

-- | Elaborate clauses and attaches type signatures. The context _must_
-- contain type signatures for all defined functions.
elabFunClauses :: Context -> [FunClause SrcSpan] -> Compiler [Ast.Declaration SrcSpan]
elabFunClauses ctx clauses = runAll $ map elabFunClause clauses
  where
    elabFunClause :: FunClause SrcSpan -> Compiler (Ast.Declaration SrcSpan)
    elabFunClause clause@(FunClause annot c ident e) = do
      let t = ctx Map.! clauseId clause
      e' <- elabExp c e `inContext` makeCompilerContext (Just annot)
              empty (text "In the definition of" <+> pPrint ident)
      return $ Ast.Declaration annot (elabIdent ident) (unLoc t) e'


-- | Elaborate a type
elabType :: Syn.Type SrcSpan -> Compiler (Ast.Type SrcSpan)
elabType (Syn.TUnit annot) = return $ Ast.TUnit annot
elabType (Syn.TProduct annot a b) =
  liftM2 (Ast.TProduct annot) (elabType a) (elabType b)
elabType (Syn.TArrow annot a b) =
  liftM2 (Ast.TArrow annot) (elabType a) (elabType b)
elabType (Syn.TInternal annot brs) = do
  checkDuplicatesOn Syn.branchLabel
    (\id -> text "Multiple types given for" <+> pPrint id) brs
  brs' <- runAll $ map (elabBranch elabType) brs
  return $ Ast.TInternal annot brs'
elabType (Syn.TExternal annot brs) = do
  checkDuplicatesOn Syn.branchLabel
    (\id -> text "Multiple types given for" <+> pPrint id) brs
  brs' <- runAll $ map (elabBranch elabType) brs
  return $ Ast.TExternal annot brs'
elabType (Syn.TIntersect annot a b) =
  liftM2 (Ast.TIntersect annot) (elabType a) (elabType b)
elabType (Syn.TUnion annot a b) =
  liftM2 (Ast.TUnion annot) (elabType a) (elabType b)


-- | Elaborate an expression. Combines multiple 'Syn.ExpLine's into one 'Ast.Exp'.
elabExp :: Syn.Channel SrcSpan -> Syn.Exp SrcSpan -> Compiler (Ast.Exp SrcSpan)
elabExp c e@(Syn.Exp annot []) = compilerError (location e)
  (text "Process expression cannot be empty:" $$ pPrint e)
elabExp c (Syn.Exp annot es) = do
  r' <- elabLast r
  foldM (flip elabLine) r' rs
  where
    r : rs = reverse es

    -- Convert a terminating expression
    elabLast :: Syn.ExpLine SrcSpan -> Compiler (Ast.Exp SrcSpan)
    elabLast (Syn.EFwd annot c' d) | c == c' =
      return $ Ast.EFwdProv annot (elabChannel d)
    elabLast p@(Syn.EFwd {}) | otherwise = compilerError (location p) $
      text "Cannot forward to non-provided channel" $$ pPrint p
    elabLast (Syn.EClose annot c') | c == c' = return $ Ast.ECloseProv annot
    elabLast p@(Syn.EClose {}) | otherwise = compilerError (location p) $
      text "Cannot close non-provided channel" $$ pPrint p
    elabLast (Syn.ECase annot d br) = do
      checkDuplicatesOn Syn.branchLabel
        ((text "Overlapping cases for" <+>) . pPrint) br
      br' <- runAll $ map branch br
      if c == d
        then return $ Ast.ECaseProv annot br'
        else return $ Ast.ECase annot (elabChannel d) br'
      where
        branch :: Syn.Branch Syn.Exp SrcSpan -> Compiler (Ast.Branch Ast.Exp SrcSpan)
        branch (Syn.Branch annot lab p) = do
          p' <- elabExp c p
          return $ Ast.Branch annot (elabLabel lab) p'
    elabLast p = compilerError (location p) $
      text "The following cannot be the last expression of a process:"
      $$ pPrint p

    -- Convert a non-terminating expression
    elabLine :: Syn.ExpLine SrcSpan -> Ast.Exp SrcSpan -> Compiler (Ast.Exp SrcSpan)
    elabLine (Syn.ESend annot c' (d, p1)) p2 | c == c' = do
      p1' <- elabExp d p1
      return $ Ast.ESendProv (mergeLocated annot p2) p1' p2
    elabLine (Syn.ESendChannel annot c' d) p | c == c' =
      return $ Ast.ESendProv (mergeLocated annot p)
        (Ast.EFwdProv (location d) (elabChannel d)) p
    elabLine (Syn.ERecv annot d c') p | c == c' =
      return $ Ast.ERecvProv (mergeLocated annot p) (elabChannel d) p
    elabLine (Syn.ESelect annot c' lab) p | c == c' =
      return $ Ast.ESelectProv (mergeLocated annot p) (elabLabel lab) p
    elabLine (Syn.ECut annot d p1 t) p2 = do
      p1' <- elabExp d p1
      t' <- elabType t
      return $ Ast.ECut (mergeLocated annot p2) (elabChannel d) p1' t' p2
    elabLine (Syn.EWait annot d) p =
      return $ Ast.EWait (mergeLocated annot p) (elabChannel d) p
    elabLine (Syn.ESend annot d (e, p1)) p2 | otherwise = do
      p1' <- elabExp e p1
      return $ Ast.ESend (mergeLocated annot p2) (elabChannel d) p1' p2
    elabLine (Syn.ESendChannel annot d e) p | otherwise =
      return $ Ast.ESend (mergeLocated annot p)
        (elabChannel d) (Ast.EFwdProv (location e) (elabChannel e)) p
    elabLine (Syn.ERecv annot d e) p | otherwise =
      return $ Ast.ERecv (mergeLocated annot p) (elabChannel d) (elabChannel e) p
    elabLine (Syn.ESelect annot d lab) p | otherwise =
      return $ Ast.ESelect (mergeLocated annot p) (elabChannel d) (elabLabel lab) p
    elabLine e1 e2 = compilerError (location e1) $
      text "The following should be the last expression in a process"
      $$ pPrint e1


elabIdent :: Syn.Ident annot -> Ast.Ident annot
elabIdent (Syn.Ident annot ident) = Ast.Ident annot ident

elabChannel :: Syn.Channel annot -> Ast.Channel annot
elabChannel (Syn.Channel annot c) = Ast.Channel annot c

elabLabel :: Syn.Label annot -> Ast.Label annot
elabLabel (Syn.Label annot lab) = Ast.Label annot lab

elabBranch :: (t1 annot -> Compiler (t2 annot))
           -> Syn.Branch t1 annot
           -> Compiler (Ast.Branch t2 annot)
elabBranch elab (Syn.Branch annot lab t) =
  liftM (Ast.Branch annot (elabLabel lab)) (elab t)


-- | Check that all elements of a list are distinct. Uses the given
-- function to extract the key to compare on.
checkDuplicatesOn :: forall key e . (Ord key, Located e, Pretty e)
                  => (e -> key)     -- ^ Extract a key to compare on
                  -> (key -> Doc)   -- ^ The message to display for duplicates
                  -> [e]            -- ^ List of inputs
                  -> Compiler ()
checkDuplicatesOn key msg es = runAll_ $ map duplicateError duplicates
  where
    -- Find duplicates
    duplicates :: [[e]]
    duplicates = filter ((>= 2) . length) $ groupBy ((==) `on` key) $ sortOn key es

    -- Raise an error for a duplicated input
    duplicateError :: [e] -> Compiler ()
    duplicateError [] = error "impossible"
    duplicateError ds@(d : _) = compilerError (location d) $
      msg (key d) <> colon $$ (nest indentation $ vcat $ map pPrint ds)

