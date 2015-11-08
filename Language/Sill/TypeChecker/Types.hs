{-# Language FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.Types
-- Description : Type representation used internally by the type-checker
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- During type checking, we split types into two syntactic groups:
-- structural types and property types. This is formalized here.
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.Types
  ( Base (..)
  , Property (..)
  , into
  , out
  ) where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import qualified Language.Sill.AST as Ast
import Language.Sill.Parser.Annotated (Annotated (..))


data Base annot = TUnit annot
                | TProduct annot (Property annot) (Property annot)
                | TArrow annot (Property annot) (Property annot)
                | TInternal annot [Ast.Branch Property annot]
                | TExternal annot [Ast.Branch Property annot]


data Property annot = TBase (Base annot)
                    | TVar annot (Ast.Constructor annot)
                    | TIntersect annot (Property annot) (Property annot)
                    | TUnion annot (Property annot) (Property annot)


{--------------------------------------------------------------------------
  Converting
--------------------------------------------------------------------------}

into :: Ast.Type annot -> Property annot
into (Ast.TVar annot con) = TVar annot con
into (Ast.TUnit annot) = TBase (TUnit annot)
into (Ast.TProduct annot a b) = TBase $ TProduct annot (into a) (into b)
into (Ast.TArrow annot a b) = TBase $ TArrow annot (into a) (into b)
into (Ast.TInternal annot brs) =
  TBase $ TInternal annot (map (Ast.branchMap into) brs)
into (Ast.TExternal annot brs) =
  TBase $ TExternal annot (map (Ast.branchMap into) brs)
into (Ast.TIntersect annot a b) = TIntersect annot (into a) (into b)
into (Ast.TUnion annot a b) = TUnion annot (into a) (into b)

out :: Property annot -> Ast.Type annot
out (TVar annot con) = Ast.TVar annot con
out (TIntersect annot a b) = Ast.TIntersect annot (out a) (out b)
out (TUnion annot a b) = Ast.TUnion annot (out a) (out b)
out (TBase b) = outBase b
  where
    outBase :: Base annot -> Ast.Type annot
    outBase (TUnit annot) = Ast.TUnit annot
    outBase (TProduct annot a b) = Ast.TProduct annot (out a) (out b)
    outBase (TArrow annot a b) = Ast.TArrow annot (out a) (out b)
    outBase (TInternal annot brs) =
      Ast.TInternal annot $ map (Ast.branchMap out) brs
    outBase (TExternal annot brs) =
      Ast.TExternal annot $ map (Ast.branchMap out) brs


{--------------------------------------------------------------------------
  Annotations
--------------------------------------------------------------------------}

instance Annotated Base where
  annot (TUnit annot) = annot
  annot (TProduct annot _ _) = annot
  annot (TArrow annot _ _) = annot
  annot (TInternal annot _) = annot
  annot (TExternal annot _) = annot

instance Annotated Property where
  annot (TVar annot _) = annot
  annot (TBase b) = annot b
  annot (TIntersect annot _ _) = annot
  annot (TUnion annot _ _) = annot


{--------------------------------------------------------------------------
  Printing
--------------------------------------------------------------------------}

instance Pretty (Base annot) where
  pPrint = pPrint . out . TBase

instance Pretty (Property annot) where
  pPrint = pPrint . out

instance Pretty (Ast.Branch Property annot) where
  pPrint (Ast.Branch annot lab t) = pPrint (Ast.Branch annot lab $ out t)


{--------------------------------------------------------------------------
  Showing
--------------------------------------------------------------------------}

instance Show (Base annot) where
  show = show . out . TBase

instance Show (Property annot) where
  show = show . out

