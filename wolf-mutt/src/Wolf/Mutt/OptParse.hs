{-# LANGUAGE RecordWildCards #-}

module Wolf.Mutt.OptParse
  ( getInstructions
  , Instructions(..)
  , Dispatch(..)
  , Settings(..)
  ) where

import Import

import qualified Data.Text as T

import System.Environment (getArgs)

import Options.Applicative

import Wolf.Cli.OptParse (deriveDataSettings, parseDataFlags')

import Wolf.Mutt.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
  Arguments cmd flags <- getArguments
  config <- getConfiguration cmd flags
  combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandQuery s) Flags {..} Configuration = do
  ds <- deriveDataSettings flagDataFlags
  let disp = DispatchQuery $ T.pack s
  pure $ Instructions disp Settings {setDataSets = ds}

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
      ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) (fullDesc <> progDesc "wolf-mutt")

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "query" parseCommandQuery]

parseCommandQuery :: ParserInfo Command
parseCommandQuery =
  info
    (CommandQuery <$> strArgument (mconcat [metavar "QUERY", help "The query"]))
    (fullDesc <> progDesc "Query for an email address")

parseFlags :: Parser Flags
parseFlags = Flags <$> parseDataFlags'
