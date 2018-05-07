{-# LANGUAGE RecordWildCards #-}

module Wolf.Email.OptParse
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

import Wolf.Email.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    Arguments cmd flags <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions CommandSuggest Flags {..} Configuration = do
    ds <- deriveDataSettings flagDataFlags
    let disp = DispatchSuggest
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
argParser = info (helper <*> parseArgs) (fullDesc <> progDesc "wolf-email")

parseArgs :: Parser Arguments
parseArgs = Arguments <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "suggest" parseCommandSuggest]

parseCommandSuggest :: ParserInfo Command
parseCommandSuggest =
    info
        (pure CommandSuggest)
        (fullDesc <> progDesc "Make suggestions based on emails")

parseFlags :: Parser Flags
parseFlags = Flags <$> parseDataFlags'
