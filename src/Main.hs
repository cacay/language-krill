
module Main where

import Control.Monad (when)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS

import Language.Sill.Parser.Location (SrcSpan)

import Language.Sill.Parser.Token (Lexeme, Token, token)
import Language.Sill.Parser.Syntax (File, Module)

import Language.Sill.Desugaring.Desugar (desugarFile)
import Language.Sill.Monad.Compiler (Compiler, runCompiler)
import qualified Language.Sill.Parser.Parser as Parser
import Language.Sill.TypeChecker.TypeChecker (checkFile)


parseTokens :: FilePath -> IO [Token]
parseTokens f = do
  code <- BS.readFile f
  case Parser.runParser code Parser.tokenParser of
    Left err -> error err
    Right e -> return e

parseFile :: FilePath -> IO (File SrcSpan)
parseFile f = do
  code <- BS.readFile f
  case Parser.runParser code (Parser.fileParser f) of
    Left err -> error err
    Right e -> return e

liftIOEither :: Show e => Either e a -> IO a
liftIOEither (Left e) = error (show e)
liftIOEither (Right a) = return a

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $
    error "Takes exactly one argument"

  putStrLn "Tokens:"
  parseTokens (head args) >>= print

  putStrLn "\nSyntax:"
  file <- parseFile (head args)
  print file

  putStrLn "\nAST:"
  elab <- liftIOEither $ runCompiler $ desugarFile file

  putStrLn "\nType checking:"
  liftIOEither $ runCompiler $ checkFile elab

  return ()

