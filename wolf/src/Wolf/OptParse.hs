module Wolf.OptParse
    ( module Wolf.OptParse
    , module Wolf.OptParse.Types
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import Wolf.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags Configuration = do
    disp <-
        case cmd of
            CommandNote person -> pure $ DispatchNote person
    pure (disp, Settings)

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
    description = "Wolf"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand = hsubparser $ mconcat [command "note" parseCommandNote]

parseCommandNote :: ParserInfo Command
parseCommandNote = info parser modifier
  where
    parser =
        CommandNote <$>
        strArgument
            (mconcat [metavar "PERSON", help "The person to make a note about."])
    modifier = fullDesc <> progDesc "Make a note."

parseFlags :: Parser Flags
parseFlags = pure Flags
