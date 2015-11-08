-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.Contractivity
-- Description : Check that recursive types are contractive
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- A (recursive) type is said to be contractive if its unfolding corresponds
-- to a (possibly infinite) tree, or equivalently, if there is a finite
-- unfolding that exposes a structural construct at the root of the type.
--
-- Since we use equirecursive types, we require all types to be contractive
-- for soundness.
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.Contractivity
  ( contractiveFile
  , contractiveModule
  ) where

import Control.Arrow ((&&&))
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map.Strict as Map

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Sill.Parser.Location (Located (..), SrcSpan)

import Language.Sill.Monad.Compiler
import Language.Sill.AST


type TypeDefs = Map.Map (Constructor SrcSpan) (TypeDef SrcSpan)

type Seen = Map.Map (Constructor SrcSpan) Bool

type Result = CompilerT (ReaderT TypeDefs (State Seen))


contractiveFile :: File SrcSpan -> Compiler ()
contractiveFile (File _ modules) = runAll_ (map contractiveModule modules)

contractiveModule :: Module SrcSpan -> Compiler ()
contractiveModule (Module _ ident typedefs _) = do
  let m = runCompilerT (runAll_ $ map checkTypeDefError typedefs)
  case evalState (runReaderT m typedefMap) Map.empty of
    Left e -> throwError e
    Right () -> return ()
  where
    conId (TypeDef _ con _) = con
    typedefMap = Map.fromList $ map (conId &&& id) typedefs


checkTypeDefError :: TypeDef SrcSpan -> Result ()
checkTypeDefError def@(TypeDef annot _ _) = do
  contractive <- checkTypeDef def
  unless contractive $ compilerError annot $
    text "Type definition is not contractive:" $$ pPrint def


checkTypeDef :: TypeDef SrcSpan -> Result Bool
checkTypeDef (TypeDef _ con t) = do
  seen <- gets (Map.lookup con)
  case seen of
    Nothing -> do
      modify (Map.insert con False)
      r <- checkType t
      modify (Map.insert con r)
      return r
    Just r -> return r

checkType :: Type SrcSpan -> Result Bool
checkType (TVar annot con) = do
  def <- asks (Map.lookup con)
  case def of
    Nothing -> compilerError annot $
      text "Undefined constructor:" <+> pPrint con
    Just typedef -> checkTypeDef typedef
checkType (TUnit {}) = return True
checkType (TProduct {}) = return True
checkType (TArrow {}) = return True
checkType (TInternal {}) = return True
checkType (TExternal {}) = return True
checkType (TIntersect _ a b) = liftM and (runAll [checkType a, checkType b])
checkType (TUnion _ a b) = liftM and (runAll [checkType a, checkType b])

