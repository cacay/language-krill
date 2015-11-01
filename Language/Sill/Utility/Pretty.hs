-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Utility.Pretty
-- Description : Helper functions for pretty printing
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Utility.Pretty where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)


{--------------------------------------------------------------------------
  Spacing
--------------------------------------------------------------------------}

-- | Default indentation level
indentation :: Int
indentation = 2

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

