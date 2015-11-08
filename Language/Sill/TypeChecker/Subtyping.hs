-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.Subtyping
-- Description : Algorithm to decide the sub-typing relation
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- TODO: Some discussion of contractive types and co-induction.
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.Subtyping
  ( TypeDefs
  , subBase
  , subProperty
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Sill.Parser.Location (Located (..), SrcSpan)

import qualified Language.Sill.AST as Ast
import Language.Sill.Monad.Compiler
import Language.Sill.TypeChecker.Types


-- | Type definitions in scope
type TypeDefs = Map.Map (Ast.Constructor SrcSpan) (Property SrcSpan)

-- Set of co-inductive hypothesis
type CoInduction = Set.Set (Ast.Constructor SrcSpan, Ast.Constructor SrcSpan)

type Result = CompilerT (ReaderT TypeDefs (State CoInduction))


-- | Decide if the base type is a subtype of the other
subBase :: TypeDefs -> Base SrcSpan -> Base SrcSpan -> Compiler ()
subBase typedefs a b = run typedefs (subBaseHelper a b)

-- | Decide if the property type is a subtype of the other
subProperty :: TypeDefs -> Property SrcSpan -> Property SrcSpan -> Compiler ()
subProperty typedefs a b = run typedefs (subPropertyHelper a b)


run :: TypeDefs -> Result a -> Compiler a
run typedefs m =
  case evalState (runReaderT (runCompilerT m) typedefs) Set.empty of
    Left e -> throwError e
    Right a -> return a


subBaseHelper :: Base SrcSpan -> Base SrcSpan -> Result ()
subBaseHelper (TUnit _) (TUnit _) = return ()
subBaseHelper (TProduct _ a1 b1) (TProduct _ a2 b2) =
  runAll_ [subPropertyHelper a1 a2, subPropertyHelper b1 b2]
subBaseHelper (TArrow _ a1 b1) (TArrow _ a2 b2) =
  runAll_ [subPropertyHelper a2 a1, subPropertyHelper b1 b2]
subBaseHelper a b = compilerError (location b) $
  text "Cannot match expected type" <+> pPrint b
    $$ text "when proving" <+> pPrint a <+> text "<=" <+> pPrint b

subPropertyHelper :: Property SrcSpan -> Property SrcSpan -> Result ()
subPropertyHelper (TBase b1) (TBase b2) = subBaseHelper b1 b2
subPropertyHelper a (TIntersect _ b1 b2) =
  runAll_ [subPropertyHelper a b1, subPropertyHelper a b2]
subPropertyHelper (TIntersect _ a1 a2) b =
  runAny_ [subPropertyHelper a1 b, subPropertyHelper a2 b]
subPropertyHelper a (TUnion _ b1 b2) =
  runAny_ [subPropertyHelper a b1, subPropertyHelper a b2]
subPropertyHelper (TUnion _ a1 a2) b =
  runAll_ [subPropertyHelper a1 b, subPropertyHelper a2 b]
subPropertyHelper (TVar _ con1) (TVar _ con2) = do
  ind <- gets (Set.member (con1, con2))
  unless ind $ do
    modify $ Set.insert (con1, con2)
    t1 <- unfold con1
    t2 <- unfold con2
    subPropertyHelper t1 t2
subPropertyHelper (TVar _ con1) b = do
  a <- unfold con1
  subPropertyHelper a b
subPropertyHelper a (TVar _ con2) = do
  b <- unfold con2
  subPropertyHelper a b


unfold :: Ast.Constructor SrcSpan -> Result (Property SrcSpan)
unfold con = do
  def <- asks (Map.lookup con)
  case def of
    Nothing -> compilerError (location con) $
      text "Undefined constructor:" <+> pPrint con
    Just prop -> return prop

