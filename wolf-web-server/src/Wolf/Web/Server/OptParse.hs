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

import System.Environment (getArgs, getEnvironment)
import Text.Read

import Options.Applicative

import Wolf.Web.Server.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    env <- getEnv
    combineToInstructions cmd flags config env

combineToInstructions ::
       Command -> Flags -> Configuration -> Environment -> IO Instructions
combineToInstructions (CommandServe ServeFlags {..}) Flags Configuration Environment {..} = do
    ds <-
        case fromMaybe (SharedFlags ".") $
             serveFlagDataFlags `mplus` envDataFlags of
            PersonalFlags fp -> PersonalSets <$> resolveDir' fp
            SharedFlags fp -> SharedSets <$> resolveDir' fp
    pure
        ( DispatchServe
              ServeSettings
              { serveSetPort =
                    fromMaybe defaultPort $ serveFlagPort `mplus` envPort
              , serveSetDataSets = ds
              }
        , Settings)

defaultPort :: Int
defaultPort = 8000

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getEnv :: IO Environment
getEnv = do
    env <- getEnvironment
    let mv k = lookup k env
    pure
        Environment
        { envPort = mv "PORT" >>= readMaybe
        , envDataFlags =
              (SharedFlags <$> mv "SHARED_DATA_DIR") `mplus`
              (PersonalFlags <$> mv "PERSONAL_DATA_DIR")
        }

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
             (mconcat
                  [ long "port"
                  , metavar "PORT"
                  , value Nothing
                  , help "the port to serve on"
                  ]) <*>
         parseDataFlags)
    modifier = fullDesc <> progDesc "Serve."

parseDataFlags :: Parser (Maybe DataFlags)
parseDataFlags =
    option
        ((Just . PersonalFlags) <$> str)
        (mconcat
             [ long "personal-data-dir"
             , metavar "DIR"
             , value Nothing
             , help "The directory to serve from, for a personal server"
             ]) <|>
    option
        ((Just . SharedFlags) <$> str)
        (mconcat
             [ long "shared-data-dir"
             , metavar "DIR"
             , value Nothing
             , help "The directory to serve from, for a shared server"
             ])

parseFlags :: Parser Flags
parseFlags = pure Flags
