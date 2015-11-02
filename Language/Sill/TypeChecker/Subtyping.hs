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

import Language.Sill.Parser.Location (Located (..), SrcSpan)
import Language.Sill.TypeChecker.Common (Result, all_, any_)
import Language.Sill.TypeChecker.Types

import Language.Sill.Utility.Error (makeError)


subBase :: Base SrcSpan -> Base SrcSpan -> Result ()
subBase (TUnit _) (TUnit _) = return ()
subBase (TProduct _ a1 b1) (TProduct _ a2 b2) =
  all_ [subPropery a1 a2, subPropery b1 b2]
subBase (TArrow _ a1 b1) (TArrow _ a2 b2) =
  all_ [subPropery a2 a1, subPropery b1 b2]
subBase a b = throwError $ makeError
  (location b)
  ("Cannot match expected type " ++ show b)
  (text "When proving " <+> pPrint a <+> text "<=" <+> pPrint b)

subPropery :: Property SrcSpan -> Property SrcSpan -> Result ()
subPropery a (TIntersect _ b1 b2) = all_ [subPropery a b1, subPropery a b2]
subPropery (TIntersect _ a1 a2) b = any_ [subPropery a1 b, subPropery a2 b]
subPropery a (TUnion _ b1 b2) = any_ [subPropery a b1, subPropery a b2]
subPropery (TUnion _ a1 a2) b = all_ [subPropery a1 b, subPropery a2 b]
subPropery (TBase b1) (TBase b2) = subBase b1 b2

