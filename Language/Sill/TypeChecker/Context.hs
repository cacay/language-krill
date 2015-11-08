{-# LANGUAGE RankNTypes, TemplateHaskell #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.Context
-- Description : Definition of and operations on the type-checking context
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.Context
  ( Context
  , empty
    -- * Sub-contexts
  , SubContext
  , typedefs
  , functions
  , channels
    -- * Context manipulation
  , null
  , member
  , lookup
  , insert
  , add
  , delete
  , split
    -- * Printing
  , prettyChannels
  ) where

import Prelude hiding (lookup, null)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as Map

import Lens.Micro
import Lens.Micro.TH (makeLenses)
import Text.PrettyPrint hiding (empty)
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

import Language.Sill.Parser.Location (Located (..), mergeLocated, SrcSpan)

import qualified Language.Sill.AST as Ast
import qualified Language.Sill.TypeChecker.Types as Types


----------------------------------------------------------------------------
-- * Shorthands
----------------------------------------------------------------------------

type Base = Types.Base SrcSpan
type Property = Types.Property SrcSpan

type Ident = Ast.Ident SrcSpan
type Constructor = Ast.Constructor SrcSpan
type Channel = Ast.Channel SrcSpan


----------------------------------------------------------------------------
-- * Definition of a context
----------------------------------------------------------------------------

data Context = Context
  { _ctxTypeDef  :: Map.Map Constructor Property
  , _ctxFunction :: Map.Map Ident Property
  , _ctxChannel  :: Map.Map Channel [Base]
  }

makeLenses '' Context


empty :: Context
empty = Context Map.empty Map.empty Map.empty


----------------------------------------------------------------------------
-- * Sub-contexts
----------------------------------------------------------------------------

newtype SubContext k v = SubContext { unSubContext :: Lens' Context (Map.Map k v) }
                         --
-- | Access the context of type definitions
typedefs :: SubContext Constructor Property
typedefs = SubContext ctxTypeDef

-- | Access the function signature context
functions :: SubContext Ident Property
functions = SubContext ctxFunction

-- | Access the channel context
channels :: SubContext Channel [Base]
channels = SubContext ctxChannel


----------------------------------------------------------------------------
-- * Context manipulation
----------------------------------------------------------------------------

-- | True if the given context is empty, false otherwise
null :: SubContext k v -> Context -> Bool
null (SubContext l) = Map.null . (^. l)

-- | Check if a key is in a context
member :: Ord k => SubContext k v -> k -> Context -> Bool
member (SubContext l) k = Map.member k . (^. l)

-- | Lookup the value of a key in a context
lookup :: Ord k => SubContext k v -> k -> Context -> Maybe v
lookup (SubContext l) k = Map.lookup k . (^. l)

-- | Insert a new (key, value) pair to the given context
insert :: Ord k => SubContext k v -> k -> v -> Context -> Context
insert (SubContext l) k v = l %~ Map.insert k v

-- | Append a new value to a list of values for a key in the given context
add :: Ord k => SubContext k [v] -> k -> v -> Context -> Context
add (SubContext l) k v = l %~ Map.insertWith (++) k [v]

-- | Remove the given key from the context
delete :: Ord k => SubContext k v -> k -> Context -> Context
delete (SubContext l) k = l %~ Map.delete k

-- | Split a context in two such that the first context contains
-- (a subset of) all keys in the given list and the second contains none.
split :: Ord k => SubContext k v -> [k] -> Context -> (Context, Context)
split (SubContext l) ks = (take &&& drop)
  where keys = Map.fromList $ map (\c -> (c, ())) ks
        take = l %~ (`Map.intersection` keys)
        drop = l %~ (Map.\\ keys)

----------------------------------------------------------------------------
-- * Printing
----------------------------------------------------------------------------

prettyChannels :: Context -> Doc
prettyChannels ctx = brackets $ vcat $ punctuate comma $
  map (uncurry typing) (Map.toList $ _ctxChannel ctx)
  where
    typing :: Channel -> [Base] -> Doc
    typing c ts = pPrint c <+> colon <+> pPrint (intersect ts)

    intersect :: [Base] -> Property
    intersect ts = foldr1 (Types.TUnion (locations ts)) $ map Types.TBase ts

    locations :: [Base] -> SrcSpan
    locations = foldr1 mergeLocated . map location

