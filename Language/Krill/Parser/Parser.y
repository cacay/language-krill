-----------------------------------------------------------------------------
-- |
-- Module      : Language.Krill.Parser.Parser
-- Description : Parser for the Krill language
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- This module provides a parser for the Krill language that is intended to be
-- used with an Alex generated parser.
-----------------------------------------------------------------------------
{
module Language.Krill.Parser.Parser
  ( -- * Parsers
    Parser
  , runParser
  , fileParser
  , moduleParser
  -- , expParser
  , tokenParser
    -- * Lexemes
  , Token.Token
  , Token.Lexeme
  , Token.token
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

import Language.Krill.Parser.Location ( SrcSpan, srcLocSpan, Located (..)
                                     , mergeLocated, Loc, makeLoc, unLoc
                                     )
import Language.Krill.Parser.Lexer
import qualified Language.Krill.Parser.Token as Token
import Language.Krill.Parser.Token (Lexeme (..), token)
import Language.Krill.Parser.Syntax
}

%name fileParser1 File
%name moduleParser Module
%name expParser Exp
%name tokenParser Tokens

%monad { Alex }
%lexer { lexer } { Lexeme _ Token.TEof }
%tokentype { Lexeme }
%error { parseError }


%token
  -- Keywords
  module       { Lexeme _ Token.TModule }
  data         { Lexeme _ Token.TData }
  type         { Lexeme _ Token.TType }
  infix        { Lexeme _ Token.TInfix }
  let          { Lexeme _ Token.TLet }
  in           { Lexeme _ Token.TIn }
  where        { Lexeme _ Token.TWhere }
  do           { Lexeme _ Token.TDo }
  case         { Lexeme _ Token.TCase }
  of           { Lexeme _ Token.TOf }

  -- Special symbols
  '('          { Lexeme _ Token.TOpenParen }
  ')'          { Lexeme _ Token.TCloseParen }
  '{'          { Lexeme _ Token.TOpenBrace }
  '}'          { Lexeme _ Token.TCloseBrace }
  ';'          { Lexeme _ Token.TSemi }
  vopen        { Lexeme _ Token.TOpenVirtualBrace }
  vclosebrace  { Lexeme _ Token.TCloseVirtualBrace } -- vclose is defined later
  vsemi        { Lexeme _ Token.TVirtualSemi }
  '.'          { Lexeme _ Token.TDot }
  ','          { Lexeme _ Token.TComma }
  '_'          { Lexeme _ Token.TWild }

  -- Reserved symbols
  '='          { Lexeme _ Token.TEqual }
  '|'          { Lexeme _ Token.TBar }
  ':'          { Lexeme _ Token.TColon }
  '->'         { Lexeme _ Token.TRightArrow }
  '<-'         { Lexeme _ Token.TLeftArrow }
  lam          { Lexeme _ Token.TLam }

  one          { Lexeme _ Token.TUnit }
  '*'          { Lexeme _ Token.TTensor }
  '+'          { Lexeme _ Token.TInternal }
  '-o'         { Lexeme _ Token.TLolli }
  '&'          { Lexeme _ Token.TExternal }
  and          { Lexeme _ Token.TIntersect }
  or           { Lexeme _ Token.TUnion }

  close        { Lexeme _ Token.TClose }
  wait         { Lexeme _ Token.TWait }
  send         { Lexeme _ Token.TSend }
  recv         { Lexeme _ Token.TRecv }


  -- Identifiers
  ident        { Lexeme _ (Token.TIdent _) }
  constructor  { Lexeme _ (Token.TConstructor _) }
  channel      { Lexeme _ (Token.TChannel _) }


-- Precedences
%left or
%left and
%right '-o'
%right '*'

-- %nonassoc '(' ')' '{' '}'


%%

{--------------------------------------------------------------------------
    Helper production rules
--------------------------------------------------------------------------}

-- An optional production
Opt(p) : p               { Just $1 }
       | {- empty -}     { Nothing }

-- Empty production
Empty : {- empty -}      { () }

-- A possibly empty list of 'p's separated by 'sep's
ListSep(p, sep) : ListSep1(p, sep)    { $1 }
                | {- empty -}         { [] }

-- A list of 'p's separated by 'sep's
ListSep1(p, sep) : ListSep1R(p, sep)  { reverse $1 }

-- A list of 'p's separated by 'sep's in reverse order
ListSep1R(p, sep) : ListSep1R(p, sep) sep p  { $3 : $1 }
                  | p                        { [$1] }

-- A list of 'p's with no separators
List(p) : ListSep(p, Empty)                 { $1 }

-- A sequence of 'p's enclosed by braces and separated by semicolons
Block(p) : vopen ListSep(p, semi) vclose    { makeLoc (mergeLocated $1 $3) $2 }
                                            -- TODO: the above is not very accurate
         | '{' ListSep(p, ';') closebrace   { makeLoc (mergeLocated $1 $3) $2 }


{--------------------------------------------------------------------------
  Layout
--------------------------------------------------------------------------}

{-  A layout block might have to be closed by a parse error. Example:
        let x = e in e'
    Here the 'let' starts a layout block which should end before the 'in'.  The
    problem is that the lexer doesn't know this, so there is no virtual close
    brace. However when the parser sees the 'in' there will be a parse error.
    This is our cue to close the layout block.
-}
vclose :: { Token.Lexeme }
vclose : vclosebrace  { $1 }
       | error        {% do popLayout;
                            loc <- getSrcLoc;
                            return $ Lexeme (srcLocSpan loc) (Token.TCloseVirtualBrace)
                      }


-- A closing brace ends a 'NoLayout' block
closebrace :: { Token.Lexeme }
closebrace : '}'      {% popLayout >> return $1 }

-- You can use concrete semi colons in a layout block started with a virtual
-- brace, so we don't have to distinguish between the two in this case
semi :: { Token.Lexeme }
semi : ';'    { $1 }
     | vsemi  { $1 }


{--------------------------------------------------------------------------
  Token Parser
--------------------------------------------------------------------------}


Tokens :: { [Token.Lexeme] }
Tokens : List(Token) { $1 }

Token :: { Token.Lexeme }
Token : module       { $1 }
      | data         { $1 }
      | type         { $1 }
      | infix        { $1 }
      | let          { $1 }
      | in           { $1 }
      | where        { $1 }
      | do           { $1 }
      | case         { $1 }
      | of           { $1 }

      -- Special symbols
      | '('          { $1 }
      | ')'          { $1 }
      | '{'          { $1 }
      | '}'          { $1 }
      | ';'          { $1 }
      | vopen        { $1 }
      | vclosebrace  { $1 }
      | vsemi        { $1 }
      | '.'          { $1 }
      | ','          { $1 }
      | '_'          { $1 }

      -- Reserved symbols
      | '='          { $1 }
      | '|'          { $1 }
      | ':'          { $1 }
      | '->'         { $1 }
      | '<-'         { $1 }
      | lam          { $1 }

      | one          { $1 }
      | '*'          { $1 }
      | '+'          { $1 }
      | '-o'         { $1 }
      | '&'          { $1 }
      | and          { $1 }
      | or           { $1 }

      | close        { $1 }
      | wait         { $1 }
      | send         { $1 }
      | recv         { $1 }

      -- Identifiers
      | ident        { $1 }
      | constructor  { $1 }
      | channel      { $1 }


{--------------------------------------------------------------------------
  Identifiers
--------------------------------------------------------------------------}

-- TODO: Qualified names should be different from identifiers
Qualified :: { Ident annot }
Qualified : constructor  { Ident (location $1) $ (\(Token.TConstructor id) -> id) (token $1) }

Ident :: { Ident annot }
Ident : ident  { Ident (location $1) $ (\(Token.TIdent id) -> id) (token $1) }

Constructor :: { Constructor annot }
Constructor : constructor
  { Constructor (location $1) $ (\(Token.TConstructor id) -> id) (token $1) }

Label :: { Label annot }
Label : ident  { Label (location $1) $ (\(Token.TIdent id) -> id) (token $1) }

Channel :: { Channel annot }
Channel : channel  { Channel (location $1) $ (\(Token.TChannel id) -> id) (token $1) }


{--------------------------------------------------------------------------
    Types & Expressions
--------------------------------------------------------------------------}

Type :: { Type SrcSpan }
Type : Constructor                        { TVar (location $1) $1 }
     | one                                { TUnit (location $1) }
     | Type '*' Type                      { TProduct (mergeLocated $1 $3) $1 $3 }
     | '+' '{' ListSep(Field, ',') '}'    { TInternal (mergeLocated $1 $4) $3 }
     | Type '-o' Type                     { TArrow (mergeLocated $1 $3) $1 $3 }
     | '&' '{' ListSep(Field, ',') '}'    { TExternal (mergeLocated $1 $4) $3 }
     | Type and Type                      { TIntersect (mergeLocated $1 $3) $1 $3 }
     | Type or Type                       { TUnion (mergeLocated $1 $3) $1 $3 }
     | '(' Type ')'                       { $2 }


-- A whole process expression (currently only supports do notation)
Exp :: { Exp SrcSpan }
Exp : do Block(ExpLine) { Exp (mergeLocated $1 $2) (unLoc $2) }
    | ExpLine           { Exp (location $1) [$1] }

-- One line of a process expression
-- TODO: Location for Cut does not include Args
ExpLine :: { ExpLine annot }
ExpLine : Channel '<-' Ident Args     { ECut (mergeLocated $1 $3) $1 $3 $4 }
        | Channel '<-' Channel        { EFwd (mergeLocated $1 $3) $1 $3 }
        | close Channel               { EClose (mergeLocated $1 $2) $2 }
        | wait Channel                { EWait (mergeLocated $1 $2) $2 }
        | send Channel '(' Channel '<-' Exp ')'
                                      { ESend (mergeLocated $1 $7) $2 ($4, $6) }
        | send Channel Channel        { ESendChannel (mergeLocated $1 $3) $2 $3 }
        | Channel '<-' recv Channel   { ERecv (mergeLocated $1 $4) $1 $4 }
        | Channel '.' Label           { ESelect (mergeLocated $1 $3) $1 $3 }
        | case Channel of Block(Case) { ECase (mergeLocated $1 $4) $2 (unLoc $4) }


Branch(sep, p) : Label sep p   { Branch (mergeLocated $1 $3) $1 $3 }

Field :: { Branch Type SrcSpan }
Field : Branch(':', Type)      { $1 }

Case :: { Branch Exp SrcSpan }
Case : Branch('->', Exp)       { $1 }


{--------------------------------------------------------------------------
    Declarations
--------------------------------------------------------------------------}

Declarations :: { Loc [Declaration SrcSpan] }
Declarations : Block(Declaration) { $1 }

Declaration :: { Declaration SrcSpan }
Declaration : TypeDef       { $1 }
            | TypeSig       { $1 }
            | FunClause     { $1 }
            -- | Data       { $1 }
            -- | Infix      { $1 }


TypeDef :: { Declaration SrcSpan }
TypeDef : type Constructor '=' Type { TypeDef (mergeLocated $1 $4) $2 $4 }

TypeSig :: { Declaration SrcSpan }
TypeSig : Ident ':' Type    { TypeSig (mergeLocated $1 $3) $1 $3 }

FunClause :: { Declaration SrcSpan }
FunClause : Channel '<-' Ident Args '=' Exp
            { FunClause (mergeLocated $1 $6) $1 $3 $4 $6 }


Args :: { [Channel SrcSpan] }
Args : List(Channel) { $1 }


{--------------------------------------------------------------------------
    Top level
--------------------------------------------------------------------------}

File :: { File SrcSpan }
File : Block(Module)    { File (location $1) (unLoc $1) }

Module :: { Module SrcSpan }
Module : module Qualified where Declarations { Module (mergeLocated $1 $4) $2 (unLoc $4) }


{

{--------------------------------------------------------------------------
  Interface
--------------------------------------------------------------------------}

-- | A monadic parser
type Parser t = Alex t

-- | Run a 'Parser' on the given 'ByteString'
runParser :: ByteString -> Parser t -> Either String t
runParser = runAlex

-- | Parser for a Krill file, which is basically a list of modules
fileParser :: FilePath -> Alex (File SrcSpan)
fileParser path = setSrcFile path >> fileParser1

-- | Parser for a Krill module
moduleParser :: Alex (Module SrcSpan)

-- | Parse tokens only and return a list of lexemes. Mainly used for debugging.
-- Note that some layout blocks (i.e. {...}) won't have closing braces since
-- some of these need to be closed by parse errors and need more contexual
-- information. We do not have this since we are treating the input as a stream
-- of tokens.
tokenParser :: Alex [Token.Lexeme]


{--------------------------------------------------------------------------
  Happy related
--------------------------------------------------------------------------}

parseError :: Lexeme -> Alex a
parseError l = lexError $ "parse error on " ++ show (token l)

}

