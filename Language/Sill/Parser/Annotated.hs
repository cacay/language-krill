{-# Language FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Parser.Annotated
-- Description : Types that contain an annotation
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Parser.Annotated (Annotated (..)) where

import Language.Sill.Parser.Location (Located (..), SrcSpan)


-- | Convenient access to annotations in annotated types.
class Annotated t where
   -- | Given an annotated type, project out its annotation value.
   annot :: t annot -> annot


instance Annotated t => Located (t SrcSpan) where
  location = location . annot

-- TODO: For some reason Loc has an Annitated instance.
-- | Annotated with source location
--instance (Annotated t, Located annot) => Located (t annot) where
--  location = location . annot

