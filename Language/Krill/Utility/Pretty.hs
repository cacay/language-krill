-----------------------------------------------------------------------------
-- |
-- Module      : Language.Krill.Utility.Pretty
-- Description : Helper functions for pretty printing
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Krill.Utility.Pretty where

import Text.PrettyPrint


{--------------------------------------------------------------------------
  Spacing
--------------------------------------------------------------------------}

-- | Default indentation level
indentation :: Int
indentation = 2

-- | A '.' character
period :: Doc
period = char '.'

-- | A new line
nl :: Doc
nl = char '\n'


{--------------------------------------------------------------------------
  Pre-defined symbols
--------------------------------------------------------------------------}

leftArrow :: Doc
leftArrow = text "<-"

rightArrow :: Doc
rightArrow = text "->"

lolli  :: Doc
lolli = text "-o"

