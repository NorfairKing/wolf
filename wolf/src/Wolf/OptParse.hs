module Wolf.OptParse
    ( module Wolf.OptParse
    , module Wolf.OptParse.Types
    ) where

import Import

import Control.Monad.Reader
import qualified Data.Map as M
import System.Environment (getArgs)

import Options.Applicative

import Wolf.Index
import Wolf.OptParse.Types
import Wolf.Types

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
            CommandSummary person -> pure $ DispatchSummary person
            CommandEntry person -> pure $ DispatchEntry person
            CommandGit args -> pure $ DispatchGit args
    pure (disp, Settings)

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    env <- getParserEnv
    let result = runArgumentsParser env args
    handleParseResult result

getParserEnv :: IO ParserEnv
getParserEnv = ParserEnv <$> getIndex

runArgumentsParser :: ParserEnv -> [String] -> ParserResult Arguments
runArgumentsParser env = execParserPure prefs_ $ runReaderT argParser env
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

argParser :: ReaderT ParserEnv ParserInfo Arguments
argParser =
    ReaderT $ \env ->
        info (helper <*> runReaderT parseArgs env) (fullDesc <> progDesc "Wolf")

-- info :: Parser a -> InfoMod a -> ParserInfo a
-- f :: ReaderT ParserEnv Parser a -> InfoMod a -> ReaderT ParserEnv ParserInfo a
parseArgs :: ReaderT ParserEnv Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: ReaderT ParserEnv Parser Command
parseCommand =
    ReaderT $ \env ->
        hsubparser $
        mconcat
            [ command "note" $ runReaderT parseCommandNote env
            , command "summary" $ runReaderT parseCommandSummary env
            , command "entry" $ runReaderT parseCommandEntry env
            , command "git" parseCommandGit
            ]

parseCommandNote :: ReaderT ParserEnv ParserInfo Command
parseCommandNote =
    ReaderT $ \env ->
        let parser =
                CommandNote <$>
                strArgument
                    (mconcat
                         [ metavar "PERSON"
                         , help "The person to make a note about."
                         , completer $ listCompleter $ peopleMap env
                         ])
            modifier = fullDesc <> progDesc "Make a note."
        in info parser modifier

parseCommandSummary :: ReaderT ParserEnv ParserInfo Command
parseCommandSummary =
    ReaderT $ \env ->
        let parser =
                CommandSummary <$>
                strArgument
                    (mconcat
                         [ metavar "PERSON"
                         , help "The person to show a summary for."
                         , completer $ listCompleter $ peopleMap env
                         ])
            modifier = fullDesc <> progDesc "Show the summary for a person."
        in info parser modifier

parseCommandEntry :: ReaderT ParserEnv ParserInfo Command
parseCommandEntry =
    ReaderT $ \env ->
        let parser =
                CommandEntry <$>
                strArgument
                    (mconcat
                         [ metavar "PERSON"
                         , help "The person to edit the entry for."
                         , completer $ listCompleter $ peopleMap env
                         ])
            modifier = fullDesc <> progDesc "Edit a person's entry"
        in info parser modifier

parseCommandGit :: ParserInfo Command
parseCommandGit =
    let parser =
            CommandGit <$>
            many
                (strArgument
                     (mconcat
                          [ metavar "ARG"
                          , help "Arguments to git"
                          , completer $ listCompleter ["status", "pull", "push"]
                          ]))
        modifier =
            fullDesc <> progDesc "Perform a git command on the wolf data."
    in info parser modifier

peopleMap :: ParserEnv -> [String]
peopleMap = map (escapeSpaces . fst) . M.toList . indexMap . parserEnvIndex
  where
    escapeSpaces = concatMap go
      where
        go ' ' = "\\ "
        go c = [c]

parseFlags :: ReaderT ParserEnv Parser Flags
parseFlags = pure Flags
