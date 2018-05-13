{-# LANGUAGE RecordWildCards #-}

module Wolf.Server.OptParse
    ( module Wolf.Server.OptParse
    , module Wolf.Server.OptParse.Types
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import Wolf.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration = do
    serveSetDataDir <- resolveDir' $ fromMaybe "." serveFlagDataDir
    let serveSetPort = fromMaybe defaultPort serveFlagPort
    pure (DispatchServe ServeSettings {..}, Settings)

defaultPort :: Int
defaultPort = 8000

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = runArgumentsParser <$> getArgs >>= handleParseResult

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
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Wolf server"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

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
             (mconcat [long "port", value Nothing, help "the port to serve on"]) <*>
         option
             (Just <$> str)
             (mconcat
                  [ long "data-dir"
                  , value Nothing
                  , help "the data directory to store data in"
                  ]))
    modifier = fullDesc <> progDesc "Serve."

parseFlags :: Parser Flags
parseFlags = pure Flags
