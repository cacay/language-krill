-----------------------------------------------------------------------------
-- |
-- Module      : Language.Krill.TypeChecker.FreeVariables
-- Description : Algorithm to decide the sub-typing relation
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Krill.TypeChecker.FreeVariables
  ( freeChannels
  , freeChannelsSet
  ) where

import qualified Data.Set as Set

import Language.Krill.AST (Exp (..), Channel, Branch, branchUnpack)


freeChannels :: Exp annot -> [Channel annot]
freeChannels = Set.toList . freeChannelsSet


freeChannelsSet :: Exp annot -> Set.Set (Channel annot)
freeChannelsSet (EFwdProv _ c) = Set.singleton c
freeChannelsSet (ECloseProv _) = Set.empty
freeChannelsSet (ESendProv _ e1 e2 ) =
  freeChannelsSet e1 `Set.union` freeChannelsSet e2
freeChannelsSet (ERecvProv _ c e) = Set.delete c (freeChannelsSet e)
freeChannelsSet (ESelectProv _ _ e) = freeChannelsSet e
freeChannelsSet (ECaseProv _ brs) = freeBranches brs
freeChannelsSet (ECut _ c ident e) = Set.delete c (freeChannelsSet e)
freeChannelsSet (EWait _ c e) = Set.insert c (freeChannelsSet e)
freeChannelsSet (ESend _ c e1 e2 ) =
  Set.insert c (freeChannelsSet e1 `Set.union` freeChannelsSet e2)
freeChannelsSet (ERecv _ d c e) = Set.insert c $ Set.delete d (freeChannelsSet e)
freeChannelsSet (ESelect _ c _ e) = Set.insert c $ freeChannelsSet e
freeChannelsSet (ECase _ c brs) = Set.insert c $ freeBranches brs


freeBranches :: [Branch Exp annot] -> Set.Set (Channel annot)
freeBranches = foldl Set.union Set.empty . map freeBranch
  where freeBranch :: Branch Exp annot -> Set.Set (Channel annot)
        freeBranch = freeChannelsSet . snd . branchUnpack

