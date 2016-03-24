{-|
Module      : Language.Krill.Utility.List
Description : Efficient implementations of some useful functions on lists
Maintainer  : Josh Acay <cacay@cmu.edu>
Stability   : experimental
-}
module Language.Krill.Utility.List
  ( allEqual
  , group2
  , collect
  , collectSort
  ) where

import Data.Function (on)
import Data.Ord (comparing)
import qualified Data.List as List


-- | True if all elements of a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x : xs) = all (== x) xs


-- | Group a list of (key, value) pairs based on the key
group2 :: Eq a => [(a, b)] -> [(a, [b])]
group2 = map gather . List.groupBy ((==) `on` fst)
  where
    gather :: [(a, b)] -> (a, [b])
    gather l = (fst (head l), map snd l)


-- | Like 'group2' but sorts on keys first to collect non-adjacent elements
collect :: Ord a => [(a, b)] -> [(a, [b])]
collect = group2 . List.sortBy (comparing fst)


-- | Like 'collect' but sorts values too
collectSort :: (Ord a, Ord b) => [(a, b)] -> [(a, [b])]
collectSort = group2 . List.sort

