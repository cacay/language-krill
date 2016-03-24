-----------------------------------------------------------------------------
-- |
-- Module      : Language.Krill.AST
-- Description : Elaborated syntax of Krill
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Krill.AST
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

import Language.Krill.Desugaring.Syntax

