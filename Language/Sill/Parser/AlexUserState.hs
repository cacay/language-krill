{-# LANGUAGE TemplateHaskell #-}

module Language.Sill.Parser.AlexUserState
  ( -- * User state
    AlexUserState
  , alexInitUserState
    -- * Layout
  , LayoutContext (..)
    -- * Lenses
  , srcFile
  , prevStartCodes
  , layoutContext
  ) where

import Lens.Micro.TH (makeLenses)

type StartCode = Int


data AlexUserState = AlexUserState
  { _srcFile        :: FilePath
  , _prevStartCodes :: [StartCode]
  , _layoutContext  :: [LayoutContext]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { _srcFile        = "<no file>"
  , _prevStartCodes = []
  , _layoutContext  = [NoLayout]
  }

-- | We need to keep track of the context to do layout. The context
-- specifies the indentation (if any) of a layout block.
data LayoutContext = NoLayout        -- ^ no layout
                   | Layout Int      -- ^ layout at specified column
                   deriving Show


makeLenses ''AlexUserState
