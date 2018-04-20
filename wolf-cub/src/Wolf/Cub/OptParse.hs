{-# LANGUAGE RecordWildCards #-}

module Wolf.Cub.OptParse
    ( module Wolf.Cub.OptParse
    , Instructions
    , Dispatch(..)
    , Settings(..)
    , RunSettings(..)
    ) where

import Import

import Control.Monad.Reader

import System.Environment (getArgs)

import Options.Applicative

import qualified Wolf.Cli.OptParse as Cli
import Wolf.Cub.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandRun RunFlags {..}) Flags Configuration = do
    ds <- Cli.deriveDataSettings flagDataFlags
    pure (DispatchRun RunSettings {runSetDataSettings = ds}, Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    env <- Cli.getParserEnv
    let result = runArgumentsParser env args
    handleParseResult result

runArgumentsParser :: Cli.ParserEnv -> [String] -> ParserResult Arguments
runArgumentsParser pe = execParserPure prefs_ $ argParser pe
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

argParser :: Cli.ParserEnv -> ParserInfo Arguments
argParser pEnv = info (helper <*> parseArgs pEnv) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Wolf cub: A ncurses interface to your Wolf data"

parseArgs :: Cli.ParserEnv -> Parser Arguments
parseArgs pEnv = (,) <$> parseCommand pEnv <*> parseFlags

parseCommand :: Cli.ParserEnv -> Parser Command
parseCommand pEnv =
    hsubparser (mconcat [command "run" $ parseCommandRun pEnv]) <|>
    parseCommandRunParser pEnv

parseCommandRun :: Cli.ParserEnv -> ParserInfo Command
parseCommandRun pEnv = info parser modifier
  where
    parser = parseCommandRunParser pEnv
    modifier = fullDesc <> progDesc "Run cub."

parseCommandRunParser :: Cli.ParserEnv -> Parser Command
parseCommandRunParser pEnv =
    CommandRun <$> (RunFlags <$> runReaderT Cli.parseDataFlags pEnv)

parseFlags :: Parser Flags
parseFlags = pure Flags
