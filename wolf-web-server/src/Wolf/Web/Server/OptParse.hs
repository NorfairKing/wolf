{-# LANGUAGE RecordWildCards #-}

module Wolf.Web.Server.OptParse
    ( getInstructions
    , Instructions
    , Dispatch(..)
    , Settings(..)
    , ServeSettings(..)
    ) where

import Import

import System.Environment (getArgs, getEnvironment)
import System.Exit
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
    dd <- resolveDir' $ fromMaybe "." $ serveFlagDataDir `mplus` envDataDir
    let port = fromMaybe defaultPort $ serveFlagPort `mplus` envPort
    let apiPort = serveFlagAPIPort `mplus` envAPIPort
    when (apiPort == Just port) $
        die "Web server port and API port must not be the same."
    pure
        ( DispatchServe
              ServeSettings
                  { serveSetPort = port
                  , serveSetDataDir = dd
                  , serveSetAPIPort = apiPort
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
            , envDataDir = mv "DATA_DIR"
            , envAPIPort = mv "API_PORT" >>= readMaybe
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
         parseDataFlags <*>
         option
             (Just <$> auto)
             (mconcat
                  [ long "api-port"
                  , value Nothing
                  , help "the port to serve the API on"
                  ]))
    modifier = fullDesc <> progDesc "Serve."

parseDataFlags :: Parser (Maybe FilePath)
parseDataFlags =
    option
        (Just <$> str)
        (mconcat
             [ long "data-dir"
             , metavar "DIR"
             , value Nothing
             , help "The directory to serve from"
             ])

parseFlags :: Parser Flags
parseFlags = pure Flags
