-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Utility.Error
-- Description : Utility functions for printing compiler errors
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Utility.Error
  ( CompilerError
  , makeError
  , combineErrors
  ) where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Language.Sill.Utility.Pretty

import Language.Sill.Parser.Location (SrcSpan)


data CompilerError = Singleton Error
                   | Join CompilerError CompilerError

data Error = Error
  { errLocation :: SrcSpan
  , errMessage :: String
  , errDetails :: Doc
  }


makeError :: SrcSpan -> String -> Doc -> CompilerError
makeError loc msg details = Singleton (Error loc msg details)

combineErrors :: CompilerError -> CompilerError -> CompilerError
combineErrors = Join


{--------------------------------------------------------------------------
  Printing
--------------------------------------------------------------------------}

instance Pretty CompilerError where
  pPrint (Singleton err) = pPrint err
  pPrint (Join err1 err2) = pPrint err1 $+$ text "" $+$ pPrint err2

instance Pretty Error where
  pPrint err = pPrint (errLocation err) <> colon
    $$ nest indentation (text (errMessage err) $$ errDetails err)


{--------------------------------------------------------------------------
  Showing
--------------------------------------------------------------------------}

instance Show CompilerError where
  show = prettyShow

