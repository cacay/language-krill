
module Main where

import Control.Monad (when)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.ByteString.Lazy as BS
import qualified System.Exit as Exit

import qualified Options.Applicative as Options
import qualified Args as Args

import Text.PrettyPrint (vcat, render)
import Text.PrettyPrint.HughesPJClass (Pretty (..))

import Language.Krill.Parser.Location (SrcSpan)
import Language.Krill.Monad.Compiler

import qualified Language.Krill.Parser.Parser as Parser
import qualified Language.Krill.Parser.Syntax as Parsed

import Language.Krill.Desugaring.Desugar (desugarFile)
import qualified Language.Krill.Desugaring.Syntax as Desugared

import qualified Language.Krill.AST as Elaborated
import Language.Krill.TypeChecker.TypeChecker (checkFile)


exitFailure :: CompilerT IO a
exitFailure = liftIO Exit.exitFailure

exitSuccess :: CompilerT IO a
exitSuccess = liftIO Exit.exitSuccess

liftEitherIO :: Either String a -> CompilerT IO a
liftEitherIO (Left e) = do liftIO $ putStrLn e; exitFailure
liftEitherIO (Right a) = return a

liftCompiler :: Compiler a -> CompilerT IO a
liftCompiler m =
  case runCompiler m of
    Left err -> throwError err
    Right a -> return a


compile :: Args.Job -> CompilerT IO ()
compile job = do
  code <- liftIO $ BS.readFile (Args.jobSource job)

  when (Args.jobStopAt job == Args.Lexer) $ do
    tokens <- liftEitherIO $ Parser.runParser code Parser.tokenParser
    let doc = vcat (map pPrint tokens)
    save "lexer" tokens -- TODO: print better
    liftIO $ putStrLn $ render doc
    exitSuccess

  parsed <- liftEitherIO $ Parser.runParser code
              (Parser.fileParser $ Args.jobSource job)
  save "parser" parsed
  when (Args.jobStopAt job == Args.Parser) $
    do liftIO $ print parsed; exitSuccess

  desugared <- liftCompiler $ desugarFile parsed
  save "desugaring" desugared
  when (Args.jobStopAt job == Args.Desugaring) $
    do liftIO $ print desugared; exitSuccess

  liftCompiler $ checkFile desugared
  when (Args.jobStopAt job == Args.TypeChecker) $
    do liftIO $ putStrLn "No type errors!"; exitSuccess

  where
    -- TODO: implement save
    save :: Pretty a => String -> a -> CompilerT IO ()
    save extension e = return ()


main :: IO ()
main = do
  job <- Options.execParser opts
  res <- runCompilerT (compile job)
  case res of
    Left err -> do print err; Exit.exitFailure
    Right () -> Exit.exitSuccess
  where
    opts :: Options.ParserInfo Args.Job
    opts = Options.info (Options.helper <*> Args.jobParser)
      ( Options.fullDesc
      Options.<> Options.progDesc "Parse and type check FILE"
      Options.<> Options.header "The Krill compiler"
      )

