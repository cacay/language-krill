-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.TypeChecker.Common
-- Description : Definitions common to multiple stages of type-checking
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.TypeChecker.Common
  (
  ) where
{--------------------------------------------------------------------------
  Helper functions
--------------------------------------------------------------------------}

-- | Succeeds if all branches succeed.
all_ :: [Result a] -> Result ()
all_ rs = do
  let errors = lefts (map runExcept rs)
  unless (null errors) $ throwError (foldl1 combineErrors errors)

-- | Succeeds if any branch succeeds. TODO: Keep all errors instead of just one
any_ :: [Result a] -> Result ()
any_ rs = do
  let (errors, succs) = partitionEithers (map runExcept rs)
  when (null succs) $ throwError (foldl1 combineErrors errors)

