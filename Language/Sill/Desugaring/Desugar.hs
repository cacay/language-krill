{-# Language FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Desugaring.Desugar
-- Description : Convert SILL syntax to simpler intermediate form
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- desugaroration combines expression lines into one expression, and puts all
-- parts of a process (type signature and all clauses) together. It also
-- checks for duplicate type signatures, duplicate or missing function
-- definitions.
-----------------------------------------------------------------------------
module Language.Sill.Desugaring.Desugar
  ( desugarFile
  , desugarModule
  ) where


import Control.Arrow ((&&&))
import Control.Monad

import Data.Function (on)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (groupBy, sortOn)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import Language.Sill.Monad.Compiler
import qualified Language.Sill.Desugaring.Syntax as Dst
import qualified Language.Sill.Parser.Syntax as Src
import Language.Sill.Parser.Annotated
import Language.Sill.Parser.Location

import Language.Sill.Utility.Pretty


type Context = Map.Map String (Loc (Dst.Type SrcSpan))


{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}

-- | Desugar a file
desugarFile :: Src.File SrcSpan -> Compiler (Dst.File SrcSpan)
desugarFile (Src.File annot ms) =
  liftM (Dst.File annot) (mapM desugarModule ms)

-- | Desugar a module
desugarModule :: Src.Module SrcSpan -> Compiler (Dst.Module SrcSpan)
desugarModule (Src.Module annot name decls) = do
  decls' <- desugarDeclarations decls
  return $ Dst.Module annot (desugarIdent name) decls'


{--------------------------------------------------------------------------
  Local data types
--------------------------------------------------------------------------}

{- We process type signatures and function clauses separately. We use the following
   data types to do so.
-}

data TypeSig annot = TypeSig annot (Src.Ident annot) (Src.Type annot)

data FunClause annot =
  FunClause annot (Src.Channel annot) (Src.Ident annot) (Src.Exp annot)


sigId :: TypeSig SrcSpan -> String
sigId (TypeSig _ (Src.Ident _ id) _) = id

clauseId :: FunClause SrcSpan -> String
clauseId (FunClause _ _ (Src.Ident _ id) _) = id


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
    $$ pPrint (Src.FunClause annot c ident exp)


{--------------------------------------------------------------------------
  Internal functions
--------------------------------------------------------------------------}

desugarDeclarations :: [Src.Declaration SrcSpan] -> Compiler [Dst.Declaration SrcSpan]
desugarDeclarations decls = do
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
  ctx <- desugarTypeSigs sigs
  desugarFunClauses ctx clauses
  where
    sigs = [TypeSig annot id t | Src.TypeSig annot id t <- decls]
    clauses = [FunClause annot c id e | Src.FunClause annot c id e <- decls]

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


-- | Desugar type signatures.
desugarTypeSigs :: [TypeSig SrcSpan] -> Compiler Context
desugarTypeSigs = liftM Map.fromList . runAll . map desugarTypeSig
  where
    desugarTypeSig :: TypeSig SrcSpan -> Compiler (String, Loc (Dst.Type SrcSpan))
    desugarTypeSig sig@(TypeSig annot ident t) = do
     t' <- desugarType t `inContext` makeCompilerContext (Just annot)
             empty (text "In the signature for" <+> pPrint ident)
     return (sigId sig, makeLoc annot t')

-- | Desugar clauses and attaches type signatures. The context _must_
-- contain type signatures for all defined functions.
desugarFunClauses :: Context -> [FunClause SrcSpan] -> Compiler [Dst.Declaration SrcSpan]
desugarFunClauses ctx clauses = runAll $ map desugarFunClause clauses
  where
    desugarFunClause :: FunClause SrcSpan -> Compiler (Dst.Declaration SrcSpan)
    desugarFunClause clause@(FunClause annot c ident e) = do
      let t = ctx Map.! clauseId clause
      e' <- desugarExp c e `inContext` makeCompilerContext (Just annot)
              empty (text "In the definition of" <+> pPrint ident)
      return $ Dst.Declaration annot (desugarIdent ident) (unLoc t) e'


-- | Desugar a type
desugarType :: Src.Type SrcSpan -> Compiler (Dst.Type SrcSpan)
desugarType (Src.TUnit annot) = return $ Dst.TUnit annot
desugarType (Src.TProduct annot a b) =
  liftM2 (Dst.TProduct annot) (desugarType a) (desugarType b)
desugarType (Src.TArrow annot a b) =
  liftM2 (Dst.TArrow annot) (desugarType a) (desugarType b)
desugarType (Src.TInternal annot brs) = do
  checkDuplicatesOn Src.branchLabel
    (\id -> text "Multiple types given for" <+> pPrint id) brs
  brs' <- runAll $ map (desugarBranch desugarType) brs
  return $ Dst.TInternal annot brs'
desugarType (Src.TExternal annot brs) = do
  checkDuplicatesOn Src.branchLabel
    (\id -> text "Multiple types given for" <+> pPrint id) brs
  brs' <- runAll $ map (desugarBranch desugarType) brs
  return $ Dst.TExternal annot brs'
desugarType (Src.TIntersect annot a b) =
  liftM2 (Dst.TIntersect annot) (desugarType a) (desugarType b)
desugarType (Src.TUnion annot a b) =
  liftM2 (Dst.TUnion annot) (desugarType a) (desugarType b)


-- | Desugar an expression. Combines multiple 'Src.ExpLine's into one 'Dst.Exp'.
desugarExp :: Src.Channel SrcSpan -> Src.Exp SrcSpan -> Compiler (Dst.Exp SrcSpan)
desugarExp c e@(Src.Exp annot []) = compilerError (location e)
  (text "Process expression cannot be empty:" $$ pPrint e)
desugarExp c (Src.Exp annot es) = do
  r' <- desugarLast r
  foldM (flip desugarLine) r' rs
  where
    r : rs = reverse es

    -- Convert a terminating expression
    desugarLast :: Src.ExpLine SrcSpan -> Compiler (Dst.Exp SrcSpan)
    desugarLast (Src.EFwd annot c' d) | c == c' =
      return $ Dst.EFwdProv annot (desugarChannel d)
    desugarLast p@(Src.EFwd {}) | otherwise = compilerError (location p) $
      text "Cannot forward to non-provided channel" $$ pPrint p
    desugarLast (Src.EClose annot c') | c == c' = return $ Dst.ECloseProv annot
    desugarLast p@(Src.EClose {}) | otherwise = compilerError (location p) $
      text "Cannot close non-provided channel" $$ pPrint p
    desugarLast (Src.ECase annot d br) = do
      checkDuplicatesOn Src.branchLabel
        ((text "Overlapping cases for" <+>) . pPrint) br
      br' <- runAll $ map branch br
      if c == d
        then return $ Dst.ECaseProv annot br'
        else return $ Dst.ECase annot (desugarChannel d) br'
      where
        branch :: Src.Branch Src.Exp SrcSpan -> Compiler (Dst.Branch Dst.Exp SrcSpan)
        branch (Src.Branch annot lab p) = do
          p' <- desugarExp c p
          return $ Dst.Branch annot (desugarLabel lab) p'
    desugarLast p = compilerError (location p) $
      text "The following cannot be the last expression of a process:"
      $$ pPrint p

    -- Convert a non-terminating expression
    desugarLine :: Src.ExpLine SrcSpan -> Dst.Exp SrcSpan -> Compiler (Dst.Exp SrcSpan)
    desugarLine (Src.ESend annot c' (d, p1)) p2 | c == c' = do
      p1' <- desugarExp d p1
      return $ Dst.ESendProv (mergeLocated annot p2) p1' p2
    desugarLine (Src.ESendChannel annot c' d) p | c == c' =
      return $ Dst.ESendProv (mergeLocated annot p)
        (Dst.EFwdProv (location d) (desugarChannel d)) p
    desugarLine (Src.ERecv annot d c') p | c == c' =
      return $ Dst.ERecvProv (mergeLocated annot p) (desugarChannel d) p
    desugarLine (Src.ESelect annot c' lab) p | c == c' =
      return $ Dst.ESelectProv (mergeLocated annot p) (desugarLabel lab) p
    desugarLine (Src.ECut annot d p1 t) p2 = do
      p1' <- desugarExp d p1
      t' <- desugarType t
      return $ Dst.ECut (mergeLocated annot p2) (desugarChannel d) p1' t' p2
    desugarLine (Src.EWait annot d) p =
      return $ Dst.EWait (mergeLocated annot p) (desugarChannel d) p
    desugarLine (Src.ESend annot d (e, p1)) p2 | otherwise = do
      p1' <- desugarExp e p1
      return $ Dst.ESend (mergeLocated annot p2) (desugarChannel d) p1' p2
    desugarLine (Src.ESendChannel annot d e) p | otherwise =
      return $ Dst.ESend (mergeLocated annot p)
        (desugarChannel d) (Dst.EFwdProv (location e) (desugarChannel e)) p
    desugarLine (Src.ERecv annot d e) p | otherwise =
      return $ Dst.ERecv (mergeLocated annot p) (desugarChannel d) (desugarChannel e) p
    desugarLine (Src.ESelect annot d lab) p | otherwise =
      return $ Dst.ESelect (mergeLocated annot p) (desugarChannel d) (desugarLabel lab) p
    desugarLine e1 e2 = compilerError (location e1) $
      text "The following should be the last expression in a process"
      $$ pPrint e1


desugarIdent :: Src.Ident annot -> Dst.Ident annot
desugarIdent (Src.Ident annot ident) = Dst.Ident annot ident

desugarChannel :: Src.Channel annot -> Dst.Channel annot
desugarChannel (Src.Channel annot c) = Dst.Channel annot c

desugarLabel :: Src.Label annot -> Dst.Label annot
desugarLabel (Src.Label annot lab) = Dst.Label annot lab

desugarBranch :: (t1 annot -> Compiler (t2 annot))
           -> Src.Branch t1 annot
           -> Compiler (Dst.Branch t2 annot)
desugarBranch desugar (Src.Branch annot lab t) =
  liftM (Dst.Branch annot (desugarLabel lab)) (desugar t)


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

