{-# Language FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Parser.Syntax
-- Description : Abstract, un-elaborated syntax of SILL
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Parser.Syntax
  ( File (..)
  , Module (..)
  , Declaration (..)
  , Type (..)
  , Exp (..)
  , ExpLine (..)
  , Ident (..)
  , Constructor (..)
  , Channel (..)
  , Label (..)
  , Branch (..)
  , branchLabel
  , branchUnpack
  , branchMap
  , branchLookup
  ) where

import Data.Function (on)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Language.Sill.Utility.Pretty

import Language.Sill.Parser.Annotated (Annotated (..))
import Language.Sill.Parser.Named (Named (..))


data File annot = File annot [Module annot]

data Module annot = Module annot (Ident annot) [Declaration annot]


data Declaration annot = TypeDef annot (Constructor annot) (Type annot)
                       | TypeSig annot (Ident annot) (Type annot)
                       | FunClause annot (Channel annot) (Ident annot) (Exp annot)

data Type annot = TVar annot (Constructor annot)
                | TUnit annot
                | TProduct annot (Type annot) (Type annot)
                | TArrow annot (Type annot) (Type annot)
                | TInternal annot [Branch Type annot]
                | TExternal annot [Branch Type annot]
                | TIntersect annot (Type annot) (Type annot)
                | TUnion annot (Type annot) (Type annot)

data Exp annot = Exp annot [ExpLine annot]

data ExpLine annot = ECut annot (Channel annot) (Ident annot)
                   | EFwd annot (Channel annot) (Channel annot)
                   | EClose annot (Channel annot)
                   | EWait annot (Channel annot)
                   | ESend annot (Channel annot) (Channel annot, Exp annot)
                   | ESendChannel annot (Channel annot) (Channel annot)
                   | ERecv annot (Channel annot) (Channel annot)
                   | ESelect annot (Channel annot) (Label annot)
                   | ECase annot (Channel annot) [Branch Exp annot]


data Ident annot = Ident annot String

data Constructor annot = Constructor annot String

data Channel annot = Channel annot String

data Label annot = Label annot String

data Branch t annot = Branch annot (Label annot) (t annot)


branchLabel :: Branch t annot -> Label annot
branchLabel (Branch _ lab _) = lab

branchUnpack :: Branch t annot -> (Label annot, t annot)
branchUnpack (Branch _ lab t) = (lab, t)

branchMap :: (t1 annot -> t2 annot) -> Branch t1 annot -> Branch t2 annot
branchMap f (Branch annot lab t) = Branch annot lab (f t)

branchLookup :: Label annot -> [Branch t annot] -> Maybe (t annot)
branchLookup lab = lookup lab . map branchUnpack


{--------------------------------------------------------------------------
  Instances
--------------------------------------------------------------------------}

instance Eq (Ident annot) where
  (==) = (==) `on` name

instance Eq (Constructor annot) where
  (==) = (==) `on` name

instance Eq (Channel annot) where
  (==) = (==) `on` name

instance Eq (Label annot) where
  (==) = (==) `on` name

instance Ord (Ident annot) where
  compare = compare `on` name

instance Ord (Constructor annot) where
  compare = compare `on` name

instance Ord (Channel annot) where
  compare = compare `on` name

instance Ord (Label annot) where
  compare = compare `on` name


{--------------------------------------------------------------------------
  Annotations
--------------------------------------------------------------------------}

instance Annotated File where
  annot (File annot _) = annot

instance Annotated Module where
  annot (Module annot _ _) = annot

instance Annotated Declaration where
  annot (TypeDef annot _ _) = annot
  annot (TypeSig annot _ _) = annot
  annot (FunClause annot _ _ _) = annot

instance Annotated Type where
  annot (TVar annot _) = annot
  annot (TUnit annot) = annot
  annot (TProduct annot _ _) = annot
  annot (TArrow annot _ _) = annot
  annot (TInternal annot _) = annot
  annot (TExternal annot _) = annot
  annot (TIntersect annot _ _) = annot
  annot (TUnion annot _ _) = annot

instance Annotated Exp where
  annot (Exp annot _) = annot

instance Annotated ExpLine where
  annot (ECut annot _ _) = annot
  annot (EFwd annot _ _) = annot
  annot (EClose annot _) = annot
  annot (EWait annot _) = annot
  annot (ESend annot _ _) = annot
  annot (ESendChannel annot _ _) = annot
  annot (ERecv annot _ _) = annot
  annot (ESelect annot _ _) = annot
  annot (ECase annot _ _) = annot


instance Annotated Ident where
  annot (Ident annot _) = annot

instance Annotated Constructor where
  annot (Constructor annot _) = annot

instance Annotated Channel where
  annot (Channel annot _) = annot

instance Annotated Label where
  annot (Label annot _) = annot

instance Annotated (Branch t) where
  annot (Branch annot _ _) = annot


{--------------------------------------------------------------------------
  Names
--------------------------------------------------------------------------}

instance Named (Module annot) where
  name (Module _ ident _) = name ident

instance Named (Declaration annot) where
  name (TypeDef _ con _) = name con
  name (TypeSig _ ident _) = name ident
  name (FunClause _ _ ident _) = name ident


instance Named (Ident annot) where
  name (Ident _ n) = n

instance Named (Constructor annot) where
  name (Constructor _ n) = n

instance Named (Channel annot) where
  name (Channel _ n) = n

instance Named (Label annot) where
  name (Label _ n) = n


{--------------------------------------------------------------------------
  Printing
--------------------------------------------------------------------------}

instance Pretty (File annot) where
  pPrint (File _ ms) = vcat (punctuate nl $ map pPrint ms)

instance Pretty (Module annot) where
  pPrint (Module _ name decls) = text "module" <+> pPrint name <+> text "where"
    $$ nest indentation (vcat $ map pPrint decls)

instance Pretty (Declaration annot) where
  pPrint (TypeDef _ con t) =
    text "" $+$ text "type" <+> pPrint con <+> text "=" <+> pPrint t
  pPrint (TypeSig _ ident t) = text "" $+$ pPrint ident <+> colon <+> pPrint t
  pPrint (FunClause _ c ident e) = pPrint c <+> leftArrow
    <+> pPrint ident <+> text "=" <+> text "do"
    $$ nest indentation (pPrint e)

-- TODO: better parens
instance Pretty (Type annot) where
  pPrint (TVar _ con) = pPrint con
  pPrint (TUnit _) = text "1"
  pPrint (TProduct _ a b) = parens (pPrint a <+> text "*" <+> pPrint b)
  pPrint (TArrow _ a b) = parens (pPrint a <+> lolli <+> pPrint b)
  pPrint (TInternal _ br) =
    text "+" <> braces (hsep $ punctuate semi $ map pPrint br)
  pPrint (TExternal _ br) =
    text "&" <> braces (hsep $ punctuate semi $ map pPrint br)
  pPrint (TIntersect _ a b) = parens (pPrint a <+> text "and" <+> pPrint b)
  pPrint (TUnion _ a b) = parens (pPrint a <+> text "or" <+> pPrint b)

instance Pretty (Exp annot) where
  pPrint (Exp _ es) = vcat (map pPrint es)

instance Pretty (ExpLine annot) where
  pPrint (ECut _ c ident) = pPrint c <+> leftArrow <+> pPrint ident
  pPrint (EFwd _ c d) = pPrint c <+> leftArrow <+> pPrint d
  pPrint (EClose _ c) = text "close" <+> pPrint c
  pPrint (EWait _ c) = text "wait" <+> pPrint c
  pPrint (ESend _ c (d, e)) =
    text "send" <+> pPrint c <+> parens (pPrint d <+> leftArrow <+> pPrint e)
  pPrint (ESendChannel _ c d) = text "send" <+> pPrint c <+> pPrint d
  pPrint (ERecv _ c d) = pPrint c <+> leftArrow <+> text "recv" <+> pPrint d
  pPrint (ESelect _ c lab) = pPrint c <> char '.' <> pPrint lab
  pPrint (ECase _ c br) = text "case" <+> pPrint c <+> text "of"
    $$ nest indentation (vcat $ map pPrint br)


instance Pretty (Ident annot) where
  pPrint (Ident _ ident) = text ident

instance Pretty (Constructor annot) where
  pPrint (Constructor _ con) = text con

instance Pretty (Channel annot) where
  pPrint (Channel _ c) = text c

instance Pretty (Label annot) where
  pPrint (Label _ lab) = text lab

instance Pretty (Branch Type annot) where
  pPrint (Branch _ lab t) = pPrint lab <+> colon <+> pPrint t

instance Pretty (Branch Exp annot) where
  pPrint (Branch _ lab e) = pPrint lab <+> rightArrow <+> pPrint e


{--------------------------------------------------------------------------
  Showing
--------------------------------------------------------------------------}

instance Show (File annot) where
  show = prettyShow

instance Show (Module annot) where
  show = prettyShow

instance Show (Declaration annot) where
  show = prettyShow

instance Show (Type annot) where
  show = prettyShow

instance Show (Exp annot) where
  show = prettyShow

instance Show (Ident annot) where
  show = prettyShow

instance Show (Constructor annot) where
  show = prettyShow

instance Show (Channel annot) where
  show = prettyShow

instance Show (Label annot) where
  show = prettyShow

