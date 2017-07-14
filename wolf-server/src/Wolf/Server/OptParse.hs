{-# LANGUAGE RecordWildCards #-}

module Wolf.Server.OptParse
    ( module Wolf.Server.OptParse
    , module Wolf.Server.OptParse.Types
    ) where

import Import

import Control.Monad.Reader

import System.Environment (getArgs)

import Options.Applicative

import qualified Wolf.Cli.OptParse as Cli
import Wolf.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags {..} Configuration = do
    ds <- Cli.deriveDataSettings flagDataFlags
    pure
        ( DispatchServe
              ServeSettings {serveSetPort = fromMaybe 8000 serveFlagPort}
        , Settings {setDataSettings = ds})

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
    description = "Wolf server"

parseArgs :: Cli.ParserEnv -> Parser Arguments
parseArgs pEnv = (,) <$> parseCommand <*> parseFlags pEnv

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "serve" parseCommandServe]

parseCommandServe :: ParserInfo Command
parseCommandServe = info parser modifier
  where
    parser =
        CommandServe <$>
        (ServeFlags <$>
         option
             (Just <$> auto)
             (mconcat [value Nothing, help "the port to serve on"]))
    modifier = fullDesc <> progDesc "Command example."

parseFlags :: Cli.ParserEnv -> Parser Flags
parseFlags pEnv = Flags <$> runReaderT Cli.parseDataFlags pEnv
