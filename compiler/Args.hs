-----------------------------------------------------------------------------
-- |
-- Module      : Language.Sill.AST
-- Description : Argument and option parsing
-- Maintainer  : coskuacay@gmail.com
-- Stability   : experimental
-----------------------------------------------------------------------------
module Args
  ( Job (..)
  , Stage (..)
  , jobParser
  ) where

import Options.Applicative

-- | A compiler job
data Job = Job
  { jobSource           :: FilePath
  -- , jobOutput           :: FilePath
  , jobStopAt           :: Stage
  , jobKeepIntermediate :: Bool
  }

-- | Compilation stage
data Stage = Lexer | Parser | Desugaring | Elaboration | TypeChecker
  deriving (Eq, Ord, Show)

completeRun :: Stage
completeRun = TypeChecker


-- | Parse a job
jobParser :: Parser Job
jobParser = Job
  <$> argument str (metavar "FILE")
  -- <*> strOption
  --     ( long "output"
  --    <> short 'o'
  --    <> metavar "FILE"
  --    <> help "Place the output into <FILE>"
  --     )
  <*> stageParser
  <*> switch
      ( long "save-temps"
     <> short 't'
     <> help "Save intermediate forms"
      )

stageParser :: Parser Stage
stageParser =
      flag completeRun Lexer
      ( long "lex"
     <> short 'l'
     <> help "Lex only (output a token stream)"
      )
  <|> flag completeRun Parser
      ( long "parse"
     <> short 'p'
     <> help "Parse only (act as a pretty printer)"
      )
  <|> flag completeRun Desugaring
      ( long "desugar"
     <> short 'd'
     <> help "Stop after desugaring"
      )
  <|> flag completeRun Elaboration
      ( long "elaborate"
     <> short 'e'
     <> help "Stop after elaboration"
      )
  <|> flag completeRun TypeChecker
      ( long "type-check"
     <> short 't'
     <> help "Stop after type-checking"
      )

