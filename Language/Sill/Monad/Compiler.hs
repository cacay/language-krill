{-# Language GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Monad.Compiler
-- Description : Compiler monad that supports error tracking
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Monad.Compiler
  ( -- * Compiler monad
    Compiler
  , runCompiler
    -- * CompilerT monad transformer
  , CompilerT
  , runCompilerT
    -- * Compiler errors
  , CompilerError
  , compilerError
  , inContext
    -- * Compilation context
  , CompilerContext
  , makeCompilerContext
    -- * Operations
  , runAll
  , runAll_
  , runAny
  , runAny_
  ) where

import Control.Applicative
import Control.Monad.Except

import Data.Functor.Identity
import Data.Either (partitionEithers)

import Text.PrettyPrint (Doc)

import Language.Sill.Monad.Error
import Language.Sill.Parser.Location (SrcSpan)


----------------------------------------------------------------------------
-- * Compiler monad
----------------------------------------------------------------------------

type Compiler = CompilerT Identity

-- | Extract the result of a Compiler computation
runCompiler :: Compiler a -> Either CompilerError a
runCompiler = runIdentity . runCompilerT


----------------------------------------------------------------------------
-- * CompilerT monad transformer
----------------------------------------------------------------------------

newtype CompilerT m a = CompilerT { unCompilerT :: ExceptT CompilerError m a }
  deriving ( Functor, Applicative, Alternative, Monad, MonadPlus
           , MonadFix, MonadTrans, MonadIO
           , MonadError CompilerError)

-- | Extract the result of a CompilerT computation
runCompilerT :: CompilerT m a -> m (Either CompilerError a)
runCompilerT = runExceptT . unCompilerT


----------------------------------------------------------------------------
-- * Operations
----------------------------------------------------------------------------

data CompilerContext = CompilerContext
  { ctxLocation :: Maybe SrcSpan
  , ctxBefore   :: Doc
  , ctxAfter    :: Doc
  }

makeCompilerContext :: Maybe SrcSpan  -- ^ Location of the surrounding context
                    -> Doc            -- ^ Message before
                    -> Doc            -- ^ Message after
                    -> CompilerContext
makeCompilerContext = CompilerContext


-- | Report a compiler error
compilerError :: Monad m
              => SrcSpan  -- ^ Error location
              -> Doc      -- ^ Detailed information about the error
              -> CompilerT m a
compilerError loc msg = throwError $ makeCompilerError loc msg


-- | Attach a context to a computation. This context is used to provide better
-- error messages.
inContext :: Monad m => CompilerT m a -> CompilerContext -> CompilerT m a
inContext m (CompilerContext loc before after) =
  m `catchError` (throwError . attachErrorContext loc before after)


-- | Run all computations from left to right even if there are failing ones.
-- The combined computation succeeds if every branch succeeds, in which
-- case we return all results. Otherwise, we collect and report all errors.
-- TODO: Implement the following traversible version
-- runAll :: (Traversable t, Monad m) => t (CompilerT m a) -> CompilerT m (t a)
runAll :: Monad m => [CompilerT m a] -> CompilerT m [a]
runAll ms = do
  (errors, succs) <- liftM partitionEithers $ lift $ mapM runCompilerT ms
  unless (null errors) $
    throwError $ mconcat errors
  return succs

-- | Like 'runAll' but ignores the result
-- runAll_ :: (Traversable t, Monad m) => t (CompilerT m a) -> CompilerT m ()
runAll_ :: Monad m => [CompilerT m a] -> CompilerT m ()
runAll_ = void . runAll

-- | Run the computations from left to right, returning the first
-- successful one. Combine and report all errors if all computations
-- fail.
runAny :: (Foldable t, Monad m) => t (CompilerT m a) -> CompilerT m a
runAny = msum

-- | Like 'runAny' but ignores the result
runAny_ :: (Foldable t, Monad m) => t (CompilerT m a) -> CompilerT m ()
runAny_ = void . runAny

-- | Like 'runAny' but returns the result of all successful computations.
-- Combines and report all errors if all computations fail.
-- TODO: implement runSome
runSome :: (Traversable t, Monad m) => t (CompilerT m a) -> CompilerT m (t a)
runSome = undefined

-- | Like 'runSome' but ignores the result
runSome_ :: (Traversable t, Monad m) => t (CompilerT m a) -> CompilerT m ()
runSome_ = void . runSome

