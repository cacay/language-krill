-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Monad.Error
-- Description : Definition of errors reported during compilation
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Monad.Error
  ( CompilerError
  , makeCompilerError
  , attachErrorContext
  ) where

import qualified Data.DList as DList

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)
import Language.Sill.Utility.Pretty

import Language.Sill.Parser.Location (SrcSpan)


data CompilerError = CompilerError (DList.DList ErrorWithContext)

data ErrorWithContext = Error ErrorInfo
                      | Context ContextInfo CompilerError

data ContextInfo = ContextInfo
  { ctxLocation :: Maybe SrcSpan
  , ctxBefore   :: Doc
  , ctxAfter    :: Doc
  }

data ErrorInfo = ErrorInfo
  { errLocation :: SrcSpan
  , errMessage  :: Doc
  }


makeCompilerError :: SrcSpan -> Doc -> CompilerError
makeCompilerError loc details = CompilerError $ DList.singleton $
  Error (ErrorInfo loc details)

attachErrorContext :: Maybe SrcSpan -> Doc -> Doc -> CompilerError -> CompilerError
attachErrorContext loc before after err = CompilerError $ DList.singleton $
  Context (ContextInfo loc before after) err


{--------------------------------------------------------------------------
  Instances
--------------------------------------------------------------------------}

instance Monoid CompilerError where
  mempty = CompilerError mempty
  mappend (CompilerError e1) (CompilerError e2) =
    CompilerError (e1 `mappend` e2)


{--------------------------------------------------------------------------
  Printing
--------------------------------------------------------------------------}

instance Pretty CompilerError where
  pPrint (CompilerError e) = case DList.toList e of
    [] -> error "Compiler error: there was an error during compilation but\
      \ no information is available"
    l -> vcat $ punctuate (text "\n") $ map pPrint l

instance Pretty ErrorWithContext where
  pPrint (Error err) = pPrint err
  pPrint (Context ctx err) =
    case ctxLocation ctx of
      Nothing -> body ctx
      Just loc -> pPrint loc <> colon $+$ nest indentation (body ctx)
    where
      body :: ContextInfo -> Doc
      body ctx | isEmpty (ctxBefore ctx) = pPrint err $+$ ctxAfter ctx
      body ctx = ctxBefore ctx $+$ nest indentation (pPrint err) $+$ ctxAfter ctx

instance Pretty ErrorInfo where
  pPrint err = pPrint (errLocation err) <> colon
    $+$ nest indentation (errMessage err)


{--------------------------------------------------------------------------
  Showing
--------------------------------------------------------------------------}

instance Show CompilerError where
  show = prettyShow

