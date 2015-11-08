-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.Parser.Parser
-- Description : Parser for the SILL language
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
--
-- This module provides a parser for the SILL language that is intended to be
-- used with an Alex generated parser.
-----------------------------------------------------------------------------
{
module Language.Sill.Parser.Parser
  ( fileParser
  , moduleParser
  , expParser
  , tokenParser
  , runParser
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

import Language.Sill.Parser.Location ( SrcSpan, srcLocSpan, Located (..)
                                     , mergeLocated, Loc, makeLoc, unLoc
                                     )
import Language.Sill.Parser.Lexer
import qualified Language.Sill.Parser.Token as Token
import Language.Sill.Parser.Token (Lexeme (..), token)
import Language.Sill.Parser.Syntax
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
  module       { Lexeme $$ Token.TModule }
  data         { Lexeme $$ Token.TData }
  type         { Lexeme $$ Token.TType }
  infix        { Lexeme $$ Token.TInfix }
  let          { Lexeme $$ Token.TLet }
  in           { Lexeme $$ Token.TIn }
  where        { Lexeme $$ Token.TWhere }
  do           { Lexeme $$ Token.TDo }
  case         { Lexeme $$ Token.TCase }
  of           { Lexeme $$ Token.TOf }

  -- Special symbols
  '('          { Lexeme $$ Token.TOpenParen }
  ')'          { Lexeme $$ Token.TCloseParen }
  '{'          { Lexeme $$ Token.TOpenBrace }
  '}'          { Lexeme $$ Token.TCloseBrace }
  ';'          { Lexeme $$ Token.TSemi }
  vopen        { Lexeme $$ Token.TOpenVirtualBrace }
  vclosebrace  { Lexeme $$ Token.TCloseVirtualBrace } -- vclose is defined later
  vsemi        { Lexeme $$ Token.TVirtualSemi }
  '.'          { Lexeme $$ Token.TDot }
  ','          { Lexeme $$ Token.TComma }
  '_'          { Lexeme $$ Token.TWild }

  -- Reserved symbols
  '='          { Lexeme $$ Token.TEqual }
  '|'          { Lexeme $$ Token.TBar }
  ':'          { Lexeme $$ Token.TColon }
  '->'         { Lexeme $$ Token.TRightArrow }
  '<-'         { Lexeme $$ Token.TLeftArrow }
  lam          { Lexeme $$ Token.TLam }

  one          { Lexeme $$ Token.TUnit }
  '*'          { Lexeme $$ Token.TTensor }
  '+'          { Lexeme $$ Token.TInternal }
  '-o'         { Lexeme $$ Token.TLolli }
  '&'          { Lexeme $$ Token.TExternal }
  and          { Lexeme $$ Token.TIntersect }
  or           { Lexeme $$ Token.TUnion }

  close        { Lexeme $$ Token.TClose }
  wait         { Lexeme $$ Token.TWait }
  send         { Lexeme $$ Token.TSend }
  recv         { Lexeme $$ Token.TRecv }


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
vclose :: { SrcSpan }
vclose : vclosebrace  { $1 }
       | error        {% popLayout >> getSrcLoc >>= return . srcLocSpan }


-- A closing brace ends a 'NoLayout' block
closebrace :: { SrcSpan }
closebrace : '}'      {% popLayout >> return $1 }

-- You can use concrete semi colons in a layout block started with a virtual
-- brace, so we don't have to distinguish between the two in this case
semi :: { SrcSpan }
semi : ';'    { $1 }
     | vsemi  { $1 }


{--------------------------------------------------------------------------
  Token Parser
--------------------------------------------------------------------------}

--TODO: Implement

Tokens :: { [Token.Token] }
Tokens : List(Token) { $1 }

Token :: { Token.Token }
Token : module       { Token.TModule }
      | data         { Token.TData }
      | type         { Token.TType }
      | infix        { Token.TInfix }
      | let          { Token.TLet }
      | in           { Token.TIn }
      | where        { Token.TWhere }
      | do           { Token.TDo }
      | case         { Token.TCase }
      | of           { Token.TOf }

      -- Special symbols
      | '('          { Token.TOpenParen }
      | ')'          { Token.TCloseParen }
      | '{'          { Token.TOpenBrace }
      | '}'          { Token.TCloseBrace }
      | ';'          { Token.TSemi }
      | vopen        { Token.TOpenVirtualBrace }
      | vclosebrace  { Token.TCloseVirtualBrace } -- vclose is defined later
      | vsemi        { Token.TVirtualSemi }
      | '.'          { Token.TDot }
      | ','          { Token.TComma }
      | '_'          { Token.TWild }

      -- Reserved symbols
      | '='          { Token.TEqual }
      | '|'          { Token.TBar }
      | ':'          { Token.TColon }
      | '->'         { Token.TRightArrow }
      | '<-'         { Token.TLeftArrow }
      | lam          { Token.TLam }

      | one          { Token.TUnit }
      | '*'          { Token.TTensor }
      | '+'          { Token.TInternal }
      | '-o'         { Token.TLolli }
      | '&'          { Token.TExternal }
      | and          { Token.TIntersect }
      | or           { Token.TUnion }

      | close        { Token.TClose }
      | wait         { Token.TWait }
      | send         { Token.TSend }
      | recv         { Token.TRecv }

      -- Identifiers
      | ident        { token $1 }
      | constructor  { token $1 }
      | channel      { token $1 }


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
     | one                                { TUnit $1 }
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
ExpLine :: { ExpLine annot }
ExpLine : Channel '<-' Exp ':' Type   { ECut (mergeLocated $1 $5) $1 $3 $5 }
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

-- TODO: Allow arguments
FunClause :: { Declaration SrcSpan }
FunClause : Channel '<-' Ident '=' Exp  { FunClause (mergeLocated $1 $5) $1 $3 $5 }


{--------------------------------------------------------------------------
    Top level
--------------------------------------------------------------------------}

File :: { File SrcSpan }
File : Block(Module)    { File (location $1) (unLoc $1) }

Module :: { Module SrcSpan }
Module : module Qualified where Declarations { Module (mergeLocated $1 $4) $2 (unLoc $4) }


{

{--------------------------------------------------------------------------
  Helpers
--------------------------------------------------------------------------}


{--------------------------------------------------------------------------
  Happy related
--------------------------------------------------------------------------}

type Parser t = Alex t

fileParser :: FilePath -> Alex (File SrcSpan)
fileParser path = setSrcFile path >> fileParser1

moduleParser :: Alex (Module SrcSpan)
tokenParser :: Alex [Token.Token]

runParser :: ByteString -> Parser t -> Either String t
runParser = runAlex


parseError :: Lexeme -> Alex a
parseError l = lexError $ "parse error on " ++ show (token l)

}

