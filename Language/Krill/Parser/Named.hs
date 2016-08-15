-----------------------------------------------------------------------------
-- |
-- Module      : Language.Krill.Parser.Named
-- Description : Types that have a name attached to them
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Krill.Parser.Named (Named (..)) where


-- | Convenient access to names in named types.
class Named t where
  -- | Get the name of a named type
  name :: t -> String

