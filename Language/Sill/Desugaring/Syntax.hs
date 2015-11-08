{-# Language FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Desugaring.Syntax
-- Description : Desugared syntax of SILL
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Desugaring.Syntax
  ( File (..)
  , Module (..)
  , TypeDef (..)
  , Function (..)
  , Type (..)
  , Exp (..)
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

import Language.Sill.Parser.Annotated (Annotated (..))
import Language.Sill.Parser.Named (Named (..))

import Language.Sill.Utility.Pretty


data File annot = File annot [Module annot]

data Module annot = Module annot (Ident annot) [TypeDef annot] [Function annot]

data TypeDef annot = TypeDef annot (Constructor annot) (Type annot)

data Function annot = Function annot (Ident annot) (Type annot) (Exp annot)


data Type annot = TVar annot (Constructor annot)
                | TUnit annot
                | TProduct annot (Type annot) (Type annot)
                | TArrow annot (Type annot) (Type annot)
                | TInternal annot [Branch Type annot]
                | TExternal annot [Branch Type annot]
                | TIntersect annot (Type annot) (Type annot)
                | TUnion annot (Type annot) (Type annot)

data Exp annot = EFwdProv annot (Channel annot)
               | ECloseProv annot
               | ESendProv annot (Exp annot) (Exp annot)
               | ERecvProv annot (Channel annot) (Exp annot)
               | ESelectProv annot (Label annot) (Exp annot)
               | ECaseProv annot [Branch Exp annot]
               | ECut annot (Channel annot) (Ident annot) (Exp annot)
               | EWait annot (Channel annot) (Exp annot)
               | ESend annot (Channel annot) (Exp annot) (Exp annot)
               | ERecv annot (Channel annot) (Channel annot) (Exp annot)
               | ESelect annot (Channel annot) (Label annot) (Exp annot)
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
  annot (Module annot _ _ _) = annot

instance Annotated TypeDef where
  annot (TypeDef annot _ _) = annot

instance Annotated Function where
  annot (Function annot _ _ _) = annot


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
  annot (EFwdProv annot _) = annot
  annot (ECloseProv annot) = annot
  annot (ESendProv annot _ _) = annot
  annot (ERecvProv annot _ _) = annot
  annot (ESelectProv annot _ _) = annot
  annot (ECaseProv annot _) = annot
  annot (ECut annot _ _ _) = annot
  annot (EWait annot _ _) = annot
  annot (ESend annot _ _ _) = annot
  annot (ERecv annot _ _ _) = annot
  annot (ESelect annot _ _ _) = annot
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
  name (Module _ ident _ _) = name ident

instance Named (TypeDef annot) where
  name (TypeDef _ con _) = name con

instance Named (Function annot) where
  name (Function _ ident _ _) = name ident


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

wildcard :: Doc
wildcard = text "_"


instance Pretty (File annot) where
  pPrint (File _ ms) = vcat (punctuate nl $ map pPrint ms)

instance Pretty (Module annot) where
  pPrint (Module _ name typedefs funcs) =
    text "module" <+> pPrint name <+> text "where"
    $$ nest indentation (vcat $ map pPrint typedefs)
    $$ nl $$ nest indentation (vcat $ punctuate nl $ map pPrint funcs)

instance Pretty (TypeDef annot) where
  pPrint (TypeDef _ con t) =
    text "type" <+> pPrint con <+> text "=" <+> pPrint t

instance Pretty (Function annot) where
  pPrint (Function _ ident t e) = pPrint ident <+> colon <+> pPrint t
    $$ pPrint ident <+> text "=" <+> pPrint e


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
  pPrint (EFwdProv _ d) = wildcard <+> leftArrow <+> pPrint d
  pPrint (ECloseProv _) = text "close" <+> wildcard
  pPrint (ESendProv _ e1 e2) = text "send" <+> wildcard <+> parens (
    wildcard <+> leftArrow <+> pPrint e1) <> semi $$ pPrint e2
  pPrint (ERecvProv _ d e) = pPrint d <+> leftArrow <+> text "recv" <+> wildcard
    <> semi $$ pPrint e
  pPrint (ESelectProv _ lab e) = wildcard <> char '.' <> pPrint lab
    <> semi $$ pPrint e
  pPrint (ECaseProv _ br) = text "case" <+> wildcard <+> text "of"
    $$ nest indentation (vcat $ map pPrint br)
  pPrint (ECut _ c ident e) = pPrint c <+> leftArrow <+> pPrint ident
    <> semi $$ pPrint e
  pPrint (EWait _ c e) = text "wait" <+> pPrint c <> semi $$ pPrint e
  pPrint (ESend _ c e1 e2) = text "send" <+> pPrint c <+> parens (
    wildcard <+> leftArrow <+> pPrint e1) <> semi $$ pPrint e2
  pPrint (ERecv _ c d e) = pPrint c <+> leftArrow <+> text "recv" <+> pPrint d
    <> semi $$ pPrint e
  pPrint (ESelect _ c lab e) = pPrint c <> char '.' <> pPrint lab
    <> semi $$ pPrint e
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

instance Show (TypeDef annot) where
  show = prettyShow

instance Show (Function annot) where
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

