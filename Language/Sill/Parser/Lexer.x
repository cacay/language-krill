-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Parser.Lexer
-- Description : Lexer for the SILL language
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- This module provides a lexer for the SILL language that is intended to be
-- used alongside a Happy generated parser. The lexer cannot handle layout
-- without input from a parser since some virtual braces need to be closed by
-- parse errors. It handles nested comments, indentation dependent layout,
-- and mixfix operators.
-----------------------------------------------------------------------------

-- TODO (Considerations)
-- * Should we allow tabs on the offside?
-- Should we force 0 indentation initially
{
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Language.Sill.Parser.Lexer
  ( -- * The main lexing function
      lexer
      -- * Lex states
    , code, layout, empty_layout, bol
      -- * Layout management
    , topLayout, pushLayout, popLayout
      -- * Alex related
    , Alex (..), AlexReturn (..), runAlex, alexScanUser
    , lexError
      -- * Source location
    , setSrcFile, getSrcLoc
    ) where

import Control.Monad (ap, liftM, when)
import Control.Monad.State (MonadState (..), gets)

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Int (Int64)

import Lens.Micro
import Lens.Micro.Mtl
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import Language.Sill.Parser.AlexUserState
import Language.Sill.Parser.Location hiding (srcFile)
import Language.Sill.Parser.Token (Token (..), Lexeme (..))
}

%wrapper "monadUserState-bytestring"

-- -----------------------------------------------------------------------------
-- Alex "Character set macros"

$wild        = \_
$newline     = [\n\r\f]

$digit       = 0-9
$bindigit    = 0-1
$octdigit    = 0-7
$hexdigit    = [ $digit A-F a-f ]

$alpha       = [ A-Z a-z ]
$alphanum    = [$alpha $digit]
$symbol      = [ \! \# \$ \% \& \* \+ \- \/ \: \< \= \> \? \[ \] \\ \^ \| \~ ]
-- These characters cannot appear as name parts
$reserved    = [ \" \' \( \) \, \. \; \@ \_ \` \{ \} ]

-- Mixfix identifiers
$mixstart    = [$alpha $symbol # [ \- \\ ]]
$mixchar     = [$alphanum $symbol]

$any         = [ $alphanum $symbol $reserved $white ]
$any_no_nl   = [ $any # $newline ]
$comment     = [ $printable $white # $newline ]

$space       = [\ ]
$tab         = \t
$white_no_nl = [$white # $newline]


-- -----------------------------------------------------------------------------
-- Alex "Regular expression macros"

@decimal     = $digit+
@binary      = 0 [bB] $bindigit+
@octal       = 0 [oO] $octdigit+
@hexadecimal = 0 [xX] $hexdigit+
@signed      = [\-]?

@natural     = @decimal | @binary | @octal | @hexadecimal
@integer     = @signed @natural

@exponent    = [eE] [\-\+]? @decimal
@float       = @decimal \. @decimal @exponent? | @decimal @exponent

-- Mixfix parts:
--  * Cannot start with a digit (to exclude numbers)
--  * Cannot start with "-" followed by a digit (to exclude signed numbers)
--  * Cannot start with "\" to allow lambdas
--  * Cannot be exactly "-"{2,} to unambiguate comments
@mixfix_part = $mixstart $mixchar*
             | \-
             | \- [$mixstart \\] $mixchar*
             | [\-]{2,} [$mixchar # \-] $mixchar*
@mixfix      = $wild? (@mixfix_part $wild)* @mixfix_part $wild?


-- -----------------------------------------------------------------------------
-- Alex "Lexing"

tokens :-

-- States:
--   - 0            : Unused since 0 is a non-descriptive name
--   - code         : Lexing code block
--   - bol          : Beginning of line.
--   - layout       : Searching for layout
--   - empty_layout : Only used to output a virtual close brace
--   - comment      : Lexing a multiline comment

-- We do not use the 0 state since it is non-descriptive
<0> ()                                         { just $ switchTo code >> pushCode layout }

-- Ignore white space (TODO: bol and layout considerations)
<code, bol, layout> $white_no_nl+              ;

-- Comments
<code, bol, layout>
  {
     [\-]{2,}                                  ;
     [\-]{2,} [$comment # $mixchar] $comment*  ;
  }
<code, bol, layout, comment>
  "{-"                           { just $ pushCode comment }
<comment>    "-}"                { just $ popCode }
<comment>    $comment            ;
<comment>    $newline            ;


<code> $newline                  { just $ pushCode bol }

-- We need to check the offside rule for the first token on each line.  We
-- should not check the offside rule for the end of file token.
<bol>
  {
     $newline                    ;
     () / { not' atEof }         { offsideRule }
  }

-- After a layout keyword there is either an open brace (no layout) or the
-- indentation of the first token decides the column of the layout block.
<layout>
  {
     $newline                    ;
     \{                          { mkTok (const TOpenBrace)
                                   `also` (pushLayout NoLayout >> popCode)
                                 }
     ()                          { newLayoutContext `also` popCode }
  }

-- The only rule for the empty_layout state. Generates a close brace.
<empty_layout> ()              { mkTok (const TCloseVirtualBrace) `also` switchTo bol }


-- Keywords
<code> module         { mkTok $ const TModule }
<code> data           { mkTok $ const TData }
<code> type           { mkTok $ const TType }
<code> infix          { mkTok $ const TInfix }
<code> let            { mkTok (const TLet) `also` pushCode layout }
<code> in             { mkTok $ const TIn }
<code> where          { mkTok (const TWhere) `also` pushCode layout }
<code> do             { mkTok (const TDo) `also` pushCode layout }
<code> case           { mkTok $ const TCase }
<code> of             { mkTok (const TOf) `also` pushCode layout }

-- Special Symbols (these cannot appear at all in identifiers)
<code> "("            { mkTok $ const TOpenParen }
<code> ")"            { mkTok $ const TCloseParen }
<code> "{"            { mkTok $ const TOpenBrace }
<code> "}"            { mkTok $ const TCloseBrace }
<code> ";"            { mkTok $ const TSemi }
<code> ","            { mkTok $ const TComma }
<code> "."            { mkTok $ const TDot }
<code> "_"            { mkTok $ const TWild }

-- Reserved symbols
<code> "="            { mkTok $ const TEqual }
<code> "|"            { mkTok $ const TBar }
<code> ":"            { mkTok $ const TColon }
<code> "->"           { mkTok $ const TRightArrow }
<code> "<-"           { mkTok $ const TLeftArrow }
<code> \\             { mkTok $ const TLam }

<code> "1"            { mkTok $ const TUnit }
<code> "*"            { mkTok $ const TTensor }
<code> "+"            { mkTok $ const TInternal }
<code> "-o"           { mkTok $ const TLolli }
<code> "&"            { mkTok $ const TExternal }
<code> and            { mkTok $ const TIntersect }
<code> or             { mkTok $ const TUnion }

<code> close          { mkTok $ const TClose }
<code> wait           { mkTok $ const TWait }
<code> send           { mkTok $ const TSend }
<code> recv           { mkTok $ const TRecv }


-- Literals
<code> @natural         { mkTok $ TNatural . read } -- TODO: cannot parse binary
<code> @integer         { mkTok $ TInteger . read } -- TODO: cannot parse binary
<code> @signed @float   { mkTok $ TRational . read }


-- Mixfix operators and identifiers
<code> @mixfix                 { mkTok TIdent }               -- TODO: do this right
<code> \` @mixfix              { mkTok (TChannel . drop 1)  } -- TODO: do this right
-- <code> @mixfix              { mkTok TMixfixPart }          -- TODO: do this right


{

-- -----------------------------------------------------------------------------
-- Alex "Types and instances"

type StartCode = Int

instance Functor Alex where
  fmap = liftM

instance Applicative Alex where
  pure = return
  (<*>) = ap

instance MonadState AlexState Alex where
  get   = Alex $ \s -> Right (s, s)
  put s = Alex $ \_ -> Right (s, ())


-- -----------------------------------------------------------------------------
-- Alex "User state"

-- | A lens that can be used to get, set, and modify the user state
userState :: Lens' AlexState AlexUserState
userState f s = (\ust -> s { alex_ust = ust}) <$> f (alex_ust s)


-- | Set source file name
setSrcFile :: FilePath -> Alex ()
setSrcFile name = userState . srcFile .= name


-- -----------------------------------------------------------------------------
-- Alex "Lexing"

alexEOF :: Alex Lexeme
alexEOF = do
  code <- alexGetStartCode
  when (code == comment) $
    lexError "unmatched comment"
  loc <- getSrcLoc
  return $ Lexeme (srcLocSpan loc) TEof


-- | Construct a 'Lexeme' given a way to construct a 'Token' from the
-- lexed string. This function automatically adds the source position.
mkTok :: (String -> Token) -> AlexAction' Lexeme
mkTok f = action (\s p -> return $ Lexeme p (f s))

-- | Return the current position in the input as a 'SrcLoc'
getSrcLoc :: Alex SrcLoc
getSrcLoc = do
  f <- use (userState . srcFile)
  AlexPn abs line col <- gets alex_pos
  return $ makeSrcLoc f abs line col

-- | Display an error message. Source position and extra information is
-- added automatically by the function.
lexError :: String -> Alex a
lexError msg = do
  pos <- getSrcLoc
  (_, c, input) <- alexGetInput
  alexError $ render $ vcat
      [ pPrint pos <> colon <+> text msg
      , text $ c : "<ERROR>"
      , if BS.null input
          then text "[end of file]"
          else text $ BS.unpack (BS.take 30 input) ++ "..."
      ]


-- -----------------------------------------------------------------------------
-- Alex "Startcode stack management"

-- | Push the current startcode on the stack and switch to a new startcode
alexPushStartCode :: StartCode -> Alex ()
alexPushStartCode s = do
  current <- alexGetStartCode
  state <- use (userState . prevStartCodes)
  alexSetState (s : current : state)

-- | Drop the current startcode and switch to the top startcode on the stack
alexPopStartCode :: Alex ()
alexPopStartCode = use (userState . prevStartCodes) >>= alexSetState

-- | Set the entire state of the lexer at once changing all startcodes.
-- Head of the list becomes the current startcode and the tail becomes
-- the stack of previous startcodes.
alexSetState :: [StartCode] -> Alex ()
alexSetState [] = lexError "No startcodes in the stack. Lexer state cannot be empty."
alexSetState (x : xs) = do
  userState . prevStartCodes .= xs
  alexSetStartCode x

-- | Get the entire state of the lexer.
alexGetState :: Alex ([StartCode])
alexGetState = do
  current <- alexGetStartCode
  state <- use (userState . prevStartCodes)
  return (current : state)

-- | Shorthand for 'alexPushStartCode'
pushCode :: StartCode -> Alex ()
pushCode = alexPushStartCode

-- | ShortHand for 'alexPopStartCode'
popCode :: Alex ()
popCode = alexPopStartCode


-- | More informative name for 'begin'
switchTo :: StartCode -> Alex ()
switchTo = alexSetStartCode


-- -----------------------------------------------------------------------------
-- Alex "Layout management"

-- | Assert that the layout context is not empty
checkLayout :: Alex ()
checkLayout = do
  ctx <- use $ userState . layoutContext
  when (null ctx) $
    lexError "layout context cannot be empty"

topLayout :: Alex LayoutContext
topLayout = do checkLayout; liftM head $ use (userState . layoutContext)

pushLayout :: LayoutContext -> Alex ()
pushLayout l = userState . layoutContext %= (l :)

popLayout :: Alex ()
popLayout = do checkLayout; userState . layoutContext %= tail


{-| Executed for the first token in each line ('bol' state). Checks the position
    of the token relative to the current layout context. If the token is

    - /to the left/ : Exit the current context and return a virtual close
    brace (stay in the 'bol' state).
    - /same column/ : Exit the 'bol' state and return a virtual semi colon.
    - /to the right/ : Exit the 'bol' state and continue lexing.

    If the current block doesn't use layout (i.e. it was started by
    'openBrace') all positions are considered to be /to the right/.
-}
offsideRule :: AlexAction' Lexeme
offsideRule inp len = do
  offs <- getOffside
  case offs of
    LT -> do popLayout; mkTok (const TCloseVirtualBrace) inp len
    EQ -> do popCode; mkTok (const TVirtualSemi) inp len
    GT -> do popCode; alexMonadScan


{-| Start a new layout context. This is one of two ways to get out of the
    'layout' state (the other is 'openBrace'). There are two possibilities:

    - The current token is to the right of the current layout context (or we're
      in a no layout context).

    - The current token is to the left of or in the same column as the current
      context.

    In the first case everything is fine and we enter a new layout context at
    the column of the current token. In the second case we have an empty layout
    block so we enter the 'empty_layout' state. In both cases we return a
    virtual open brace without consuming any input.
-}
newLayoutContext :: AlexAction' Lexeme
newLayoutContext inp len = do
  offset <- liftM srcCol getSrcLoc
  ctx <- topLayout
  case ctx of
    Layout prevOffs | prevOffs >= offset -> pushCode empty_layout
    _ -> pushLayout (Layout offset)
  mkTok (const TOpenVirtualBrace) inp len


-- | Compute the relative position of a location to the
-- current layout context.
getOffside :: Alex Ordering
getOffside = do
  loc <- getSrcLoc
  ctx <- topLayout
  return $ case ctx of
    Layout n    -> compare (srcCol loc) n
    _           -> GT


-- -----------------------------------------------------------------------------
-- Alex "Useful combinators for actions"

-- | Nicer interface for Alex actions
type Action result = String -> SrcSpan -> Alex result

-- | Alex uses Int in its native definition of AlexAction even though it
-- internally expects an Int64
type AlexAction' result = AlexInput -> Int64 -> Alex result

-- | Nicer interface for Alex actions
action :: Action result -> AlexAction' result
action act (_, _, input) len = do
  span <- return makeSrcSpanLength `ap` getSrcLoc `ap` return (fromIntegral len)
  let str = BS.unpack $ BS.take len input
  act str span

-- | Execute the Alex monad ignoring the current lexeme
-- just :: Alex a -> AlexAction result
just m _ _ = m >> alexMonadScan

-- | Execute the monad and return the action
also :: AlexAction' result -> Alex a -> AlexAction' result
(act `also` m) input len = m >> act input len


-- -----------------------------------------------------------------------------
-- Alex "Predicates"

-- Sometimes regular expressions aren't enough. Alex provides a way to do
-- arbitrary computations to see if the input matches. This is done with a
-- lex predicate.
type AlexPredicate = () -> AlexInput -> Int -> AlexInput -> Bool

-- | Conjunction of 'AlexPredicate's.
(.&&.) :: AlexPredicate -> AlexPredicate -> AlexPredicate
p1 .&&. p2 = \x y z u -> p1 x y z u && p2 x y z u

-- | Disjunction of 'AlexPredicate's.
(.||.) :: AlexPredicate -> AlexPredicate -> AlexPredicate
p1 .||. p2 = \x y z u -> p1 x y z u || p2 x y z u

-- | Negation of 'AlexPredicate's.
not' :: AlexPredicate -> AlexPredicate
not' p = \x y z u -> not (p x y z u)

-- | True if we are at the end of the file
atEof :: AlexPredicate
atEof _ _ _ (_, _, rest) = BS.null rest


-- -----------------------------------------------------------------------------
-- Alex "Running the lexer"

-- | Thread with Happy
lexer :: (Lexeme -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

}
