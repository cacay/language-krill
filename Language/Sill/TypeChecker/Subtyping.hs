-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.Subtyping
-- Description : Algorithm to decide the sub-typing relation
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.Subtyping
  ( subBase
  , subPropery
  ) where


import Control.Monad.Except

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Sill.Monad.Compiler
import Language.Sill.Parser.Location (Located (..), SrcSpan)
import Language.Sill.TypeChecker.Types


subBase :: Base SrcSpan -> Base SrcSpan -> Compiler ()
subBase (TUnit _) (TUnit _) = return ()
subBase (TProduct _ a1 b1) (TProduct _ a2 b2) =
  runAll_ [subPropery a1 a2, subPropery b1 b2]
subBase (TArrow _ a1 b1) (TArrow _ a2 b2) =
  runAll_ [subPropery a2 a1, subPropery b1 b2]
subBase a b = compilerError (location b) $
  text "Cannot match expected type" <+> pPrint b
    $$ text "when proving" <+> pPrint a <+> text "<=" <+> pPrint b

subPropery :: Property SrcSpan -> Property SrcSpan -> Compiler ()
subPropery a (TIntersect _ b1 b2) = runAll_ [subPropery a b1, subPropery a b2]
subPropery (TIntersect _ a1 a2) b = runAny_ [subPropery a1 b, subPropery a2 b]
subPropery a (TUnion _ b1 b2) = runAny_ [subPropery a b1, subPropery a b2]
subPropery (TUnion _ a1 a2) b = runAll_ [subPropery a1 b, subPropery a2 b]
subPropery (TBase b1) (TBase b2) = subBase b1 b2

