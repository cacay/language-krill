-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Parser.Location
-- Description : Source location and span information
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Language.Sill.Parser.Location
  (
    -- * Source location
    SrcLoc
  , makeSrcLoc
  , srcFile, srcAbs, srcLine, srcCol
    -- * Source span
  , SrcSpan
  , makeSrcSpan
  , srcLocSpan
  , makeSrcSpanLength
  , mergeSrcSpan
  , spanFile, spanSLine, spanSCol, spanELine, spanECol
    -- * Types with location information
  , Located (..)
  , mergeLocated
  , Loc
  , makeLoc, unLoc
  ) where

import Data.Function (on)

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)


-- | Represents a single point within a file. Refer to 'locInvariant'.
data SrcLoc = SrcLoc
  { srcFile :: !FilePath -- ^ path to the source file
  , srcAbs  :: !Int      -- ^ absolute character offset
  , srcLine :: !Int      -- ^ line number, counting from 1
  , srcCol  :: !Int      -- ^ column number, counting from 1
  }
  deriving (Eq, Ord)

-- | Construct a 'SrcLoc' given the file, absolute character offset,
-- line number, and column number
makeSrcLoc :: FilePath -> Int -> Int -> Int -> SrcLoc
makeSrcLoc = SrcLoc

locInvariant :: SrcLoc -> Bool
locInvariant s = srcAbs s > 0 && srcLine s > 0 && srcCol s > 0


-- | Delimits a portion of a text file. The end position is defined
-- to be the column /after/ the end of the span. That is, a span of
-- (1,1)-(1,2) is one character long, and a span of (1,1)-(1,1) is
-- zero characters long.
data SrcSpan = SrcSpan
  { spanFile  :: !FilePath
  , spanSLine :: !Int
  , spanSCol  :: !Int
  , spanELine :: !Int
  , spanECol  :: !Int
  }
  deriving (Eq, Ord)

-- | Construct a span using a start location and an end location.
-- Both locations need to have the same source file. Also note
-- 'spanInvariant'.
makeSrcSpan :: SrcLoc -> SrcLoc -> SrcSpan
makeSrcSpan start end = SrcSpan
  { spanFile  = srcFile start
  , spanSLine = srcLine start
  , spanSCol  = srcCol start
  , spanELine = srcLine end
  , spanECol  = srcCol end
  }

-- | Construct a span using a start location and the number of characters
-- in the span. The span will start and end on the same line.
makeSrcSpanLength :: SrcLoc -> Int -> SrcSpan
makeSrcSpanLength s l = makeSrcSpan s $ s { srcCol = l + srcCol s }

-- | Create a 'SrcSpan' corresponding to a single point
srcLocSpan :: SrcLoc -> SrcSpan
srcLocSpan loc = makeSrcSpan loc loc

-- All 'SrcSpan' instances should satisfy this invariant
spanInvariant :: SrcSpan -> Bool
spanInvariant s = spanSLine s <= spanELine s && spanSCol s <= spanECol s


{--------------------------------------------------------------------------
  Operations
--------------------------------------------------------------------------}

-- | Fuse two spans together. Both spans need to be in the same file.
mergeSrcSpan :: SrcSpan -> SrcSpan -> SrcSpan
mergeSrcSpan s1 s2 | s1 > s2 = mergeSrcSpan s2 s1
mergeSrcSpan s1 s2 = SrcSpan
  { spanFile  = spanFile s1
  , spanSLine = spanSLine s1
  , spanSCol  = spanSCol s1
  , spanELine = spanELine s2
  , spanECol  = spanECol s2
  }


{--------------------------------------------------------------------------
  Located class
--------------------------------------------------------------------------}

-- | An object with an attached 'SrcSpan'
class Located t where
  location :: t -> SrcSpan

instance Located SrcSpan where
  location = id

instance Located (Loc a) where
  location (Loc s _) = s


-- | Marge the 'SrcSpan's of two Located objects
mergeLocated :: (Located t1, Located t2) => t1 -> t2 -> SrcSpan
mergeLocated t1 t2 = mergeSrcSpan (location t1) (location t2)


-- | Default way to attach location information
data Loc a = Loc SrcSpan a

makeLoc :: SrcSpan -> a -> Loc a
makeLoc = Loc

-- | Get the data out of a 'Loc'
unLoc :: Loc a -> a
unLoc (Loc _ a) = a


{--------------------------------------------------------------------------
  Printing
--------------------------------------------------------------------------}

instance Pretty SrcLoc where
  pPrint (SrcLoc f _ l c) = text f <> colon <> pPrint l <> comma <> pPrint c

instance Pretty SrcSpan where
  pPrint s = text (spanFile s) <> colon <> start <> text "-" <> end
    where
      SrcSpan { spanSLine = sl, spanSCol = sc
              , spanELine = el, spanECol = ec } = s

      start :: Doc
      start = pPrint sl <> comma <> pPrint sc

      end :: Doc
      end | sl == el  = pPrint ec
          | otherwise = pPrint el <> comma <> pPrint ec

instance Pretty e => Pretty (Loc e) where
  pPrint l = pPrint (unLoc l) <+> parens (text "at" <+> pPrint (location l))


instance Show SrcLoc where
  show = prettyShow

instance Show SrcSpan where
  show = prettyShow

instance Show e => Show (Loc e) where
  show l = show (unLoc l) ++ " (at " ++ show (location l) ++ ")"

