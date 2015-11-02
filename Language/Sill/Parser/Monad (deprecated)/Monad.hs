-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Parser.Monad
-- Description : A generic monad that can be used with Alex
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- Defines a monad that acts like the monadUserState-bytestring wrapper
-- except it maintains a stack of startcodes instead of just the current
-- one. Most identifier names are left unchanged to maintain compatibility
-- with the default Alex monad.
--
-- Your lexer must define the type @AlexUserState$ and the initialization
-- function @alexInitUserState@. Unfortunately, Haskell does not support
-- parametrized modules, so we need to hardcode the lexer module generated
-- by Alex in this one, and make these two modules mutually recursive.
--
--
-- NOTE: This module is INCONPLETE. It requires too much reinventing the
-- wheel. I think maintaining the stack of start codes in the UserState is
-- an easier approach for what we are doing here.
-----------------------------------------------------------------------------
module Language.Sill.Parser.Monad
  ( -- * The Alex monad
    Alex
  , AlexState
  , StartCode
  , AlexPosn (..)
  , LexError (..)
    -- * Running the parser
  , runAlex
  , alexInitState
  , alexInitPos
  , alexError
  , alexMonadScan
    -- * Manipulating the state
  , alexSetPos, alexGetPos
  , alexSetPrevPos, alexGetPrevPos
  , alexSetPrevToken, alexGetPrevToken
  , alexGetStartCode, alexSetStartCode
  , alexPushStartCode, alexPopStartCode
  , alexGetState, alexSetState
  , alexGetUserState, alexSetUserState
    -- * Useful token actions
  )
  where

import Control.Applicative (Applicative (..))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad (ap)

import Data.Int (Int64)
import Data.Tuple (fst, snd)
import qualified Data.ByteString.Lazy as ByteString

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..), prettyShow)

-- We hardcode the lexer module to access the user state
import {-# SOURCE #-} Language.Sill.Parser.Lexer ( AlexUserState
                                                 , alexInitUserState
                                                 , alexEOF
                                                 , alexScan
                                                 )


type Input = ByteString.ByteString


{--------------------------------------------------------------------------
  Monad
--------------------------------------------------------------------------}

-- | The lexer monad.
newtype Alex a = Alex { unAlex :: AlexState -> Either LexError (AlexState, a) }

-- | The lexer state.
data AlexState = AlexState
  { alex_pos        :: !AlexPosn     -- ^ position at current input location
  , alex_prev_pos   :: !AlexPosn     -- ^ position of last token
  , alex_bpos       :: !Int64        -- ^ bytes consumed so far
  , alex_inp        :: Input         -- ^ the current input
  , alex_chr        :: !Char         -- ^ the character before the input
  , alex_prev_token :: String        -- ^ the previous token
  , alex_scd        :: !StartCode    -- ^ the current startcode
  , alex_state      :: [!StartCode]  -- ^ stack of previous startcodes
  , alex_ust        :: AlexUserState -- ^ extra user state
  }

-- | Alex startcodes (lexer states)
type StartCode = Int

-- | Represents a point in the input.
--
-- If two positions have the same 'srcFile' and 'posAbs' components,
-- then the final two components should be the same as well, but since
-- this can be hard to enforce the program should not rely too much on
-- the last two components; they are mainly there to improve error
-- messages for the user.
data AlexPosn = AlexPosn
  { srcFile :: !(Maybe FilePath) -- ^ path of the source file
  , posAbs  :: !Int64            -- ^ absolute character offset
  , posLine :: !Int64            -- ^ line number, counting from 1
  , posCol  :: !Int64            -- ^ column number, counting from 1
  }

-- | What you get if parsing fails.
data LexError = LexError
  { errPos        :: AlexPosn  -- ^ where the error occured
  , errInput      :: Input     -- ^ the remaining input
  , errPrevToken  :: String    -- ^ the previous token
  , errMsg        :: String    -- ^ hopefully an explanation of what happened
  }


{--------------------------------------------------------------------------
  Instances
--------------------------------------------------------------------------}

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure = return
  (<*>) = ap

instance Monad Alex where
  return a = Alex $ \s -> Right (s, a)

  m >>= k  = Alex $ \s -> case unAlex m s of
                            Left err -> Left err
                            Right (s', a) -> unAlex (k a) s'

  -- TODO: Should 'errPos' be the last position or the current position?
  -- Agda uses 'alex_prev_pos' for some reason
  fail msg = Alex $ \s -> Left $ LexError
    { errPos       = alex_pos s
    , errInput     = alex_inp s
    , errPrevToken = alex_prev_token s
    , errMsg       = msg
    }

instance MonadError LexError Alex where
  throwError e = Alex $ \_ -> Left e

  Alex m `catchError` h = Alex $ \s ->
    case m s of
      Left err -> unAlex (h err) s
      m'              -> m'

instance MonadState AlexState Alex where
  get   = Alex $ \s -> Right s s
  put s = Alex $ \_ -> Right s ()


instance Pretty AlexPosn where
  pPrint (AlexPosn Nothing  _ l c) = pPrint l <> pPrint "," <> pPrint c
  pPrint (AlexPosn (Just f) _ l c) =
    pPrint f <> pPrint ":" <> pPrint l <> pPrint "," <> pPrint c

instance Show LexError where
  show = prettyShow

instance Pretty LexError where
  pPrint err = vcat
      [ pPrint (errPos err) <> colon <+> text (errMsg err)
      , text $ errPrevToken err ++ "<ERROR>"
      , text $ take 30 (errInput err) ++ "..."
      ]


{--------------------------------------------------------------------------
    Running the lexer
--------------------------------------------------------------------------}

-- | Default start position given the file path
alexStartPos :: Maybe FilePath -> AlexPosn
alexStartPos file = AlexPosn file 1 1 1

-- | Initial state of the parser given the file path and the input
alexInitState :: Maybe FilePath -> Input -> AlexState
alexInitState file input = AlexState
  { alex_pos        = pos
  , alex_prev_pos   = pos
  , alex_bpos       = posAbs pos
  , alex_inp        = input
  , alex_chr        = '\n'
  , alex_prev_token = ""
  , alex_scd        = 0
  , alex_state      = []
  , alex_ust        = alexInitUserState
  }
  where pos = alexStartPos file

-- | Run Alex with the default initial state on an 'Input'
runAlex :: Maybe FilePath -> Input -> Alex a -> Either LexError a
runAlex file input (Alex f) = f (alexInitState file) >>= snd


{--------------------------------------------------------------------------
    Manipulating the state
--------------------------------------------------------------------------}

alexSetPos :: AlexPosn -> Alex ()
alexSetPos p = modify $ \s -> s { alex_pos = p}

alexGetPos :: Alex AlexPosn
alexGetPos = gets alex_pos

alexSetPrevPos :: AlexPosn -> Alex ()
alexSetPrevPos p = modify $ \s -> s { alex_prev_pos = p}

alexGetPrevPos :: Alex AlexPosn
alexGetPrevPos = gets alex_prev_pos

alexSetPrevToken :: String -> Alex ()
alexSetPrevToken t = modify $ \s -> s { alex_prev_token = t }

alexGetPrevToken :: Alex String
alexGetPrevToken = gets alex_prev_token

alexSetStartCode :: StartCode -> Alex StartCode
alexSetStartCode c = modify $ \s -> s { alex_scd = c }

alexGetStartCode :: Alex StartCode
alexGetStartCode = gets alex_scd

alexPushStartCode :: StartCode -> Alex ()
alexPushStartCode s = do
  current <- alexGetStartCode
  state <- gets alex_state
  alexSetState (s : current : state)

alexPopStartCode :: Alex ()
alexPopStartCode = gets alex_state >>= alexSetState

-- | Set the entire state of the lexer at once changing all startcodes.
-- Head of the list becomes the current startcode and the tail becomes
-- the stack of previous startcodes. Head of the list is the top of the
-- stack.
alexSetState :: [StartCode] -> Alex ()
alexSetState [] = fail "No startcodes in the stack. Lexer state cannot be empty."
alexSetState (x : xs) = do
  modify $ \s -> s { alex_state = xs }
  alexSetStartCode x

-- | Get the entire state of the lexer.
alexGetState :: Alex ([StartCode])
alexGetState = do
  current <- alexGetStartCode
  state <- gets alex_state
  return (current : state)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState s = modify $ \s -> s { alex_ust = s }

alexGetUserState :: Alex AlexUserState
alexGetUserState = gets alex_ust

alexGetInput :: Alex AlexInput
alexGetInput
 = Alex $ \s@AlexState{alex_pos=pos,alex_bpos=bpos,alex_chr=c,alex_inp=inp} ->
        Right (s, (pos,c,inp,bpos))

alexSetInput :: AlexInput -> Alex ()
alexSetInput (pos,c,inp,bpos)
 = Alex $ \s -> case s{alex_pos=pos,
                       alex_bpos=bpos,
                       alex_chr=c,
                       alex_inp=inp} of
                  s@(AlexState{}) -> Right (s, ())

alexError :: String -> Alex a
alexError = fail


{--------------------------------------------------------------------------
    Lex actions
--------------------------------------------------------------------------}

--

-- | Scan the input to find the next token. Calls 'alexScanUser'.
-- This is the main lexing function where all the work happens.
alexMonadScan :: Alex Token
alexMonadScan = do
  inp@(_,_,str,n) <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError ((AlexPn _ line column),_,_,_) -> alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan
    AlexToken inp'@(_,_,_,n') _ action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len
      where
        len = n' - n


{--------------------------------------------------------------------------
    Useful token actions
--------------------------------------------------------------------------}

type AlexAction result = String -> Int64 -> Alex result

-- just ignore this token and scan another one
-- skip :: AlexAction result
skip input len = alexMonadScan

-- ignore this token, but set the start code to a new value
-- begin :: Int -> AlexAction result
begin code input len = do alexSetStartCode code; alexMonadScan

-- perform an action for this token, and set the start code to a new value
andBegin :: AlexAction result -> Int -> AlexAction result
(action `andBegin` code) input len = do alexSetStartCode code; action input len

token :: (String -> Int64 -> token) -> AlexAction token
token t input len = return (t input len)
