{-# LANGUAGE RecordWildCards #-}

module Wolf.Web.Server.OptParse
    ( getInstructions
    , Instructions
    , Dispatch(..)
    , Settings(..)
    , ServeSettings(..)
    , DataSettings(..)
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import Wolf.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration = do
    ds <-
        case fromMaybe (SharedFlags ".") serveFlagDataFlags of
            PersonalFlags fp -> PersonalSets <$> resolveDir' fp
            SharedFlags fp -> SharedSets <$> resolveDir' fp
    pure
        ( DispatchServe
              ServeSettings
              { serveSetPort = fromMaybe defaultPort serveFlagPort
              , serveSetDataSets = ds
              }
        , Settings)

defaultPort :: Int
defaultPort = 8000

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
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Wolf web server"

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
         (Just <$> parseDataFlags))
    modifier = fullDesc <> progDesc "Serve."

parseDataFlags :: Parser DataFlags
parseDataFlags =
    (PersonalFlags <$>
     strOption
         (mconcat
              [ long "personal-data-dir"
              , help "The directory to serve from, for a personal server"
              ])) <|>
    (SharedFlags <$>
     strOption
         (mconcat
              [ long "shared-data-dir"
              , help "The directory to serve from, for a shared server"
              ]))

parseFlags :: Parser Flags
parseFlags = pure Flags
