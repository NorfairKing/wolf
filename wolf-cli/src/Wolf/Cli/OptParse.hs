{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Cli.OptParse
  ( module Wolf.Cli.OptParse
  , module Wolf.Cli.OptParse.Types
  ) where

import Import

import qualified Data.Map as M
import qualified Data.Text as T

import System.Environment (getArgs)

import Options.Applicative

import Wolf.Cli.OptParse.Types
import Wolf.Data

getInstructions :: IO Instructions
getInstructions = do
  (cmd, flags) <- getArguments
  config <- getConfiguration cmd flags
  combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags {..} Configuration = do
  ds <- deriveDataSettings flagDataFlags
  disp <-
    case cmd of
      CommandInit -> pure DispatchInit
      CommandNote mp people ->
        pure $
        DispatchNote $
        map alias $
        case mp of
          Just p -> p : people
          Nothing -> people
      CommandSummary person -> pure $ DispatchSummary $ alias person
      CommandEntry person -> pure $ DispatchEntry $ alias person
      CommandGit args -> pure $ DispatchGit args
      CommandAlias old new -> pure $ DispatchAlias (alias old) (alias new)
      CommandReview mpd -> pure $ DispatchReview $ fromMaybe LastWeek mpd
      CommandRandomPerson -> pure DispatchRandomPerson
      CommandSuggestion sfs ->
        case sfs of
          CommandListSuggestions -> pure $ DispatchSuggestion DispatchListSuggestions
          CommandReviewSuggestion -> pure $ DispatchSuggestion DispatchReviewSuggestion
      CommandExport -> pure DispatchExport
      CommandCleanup -> pure DispatchCleanup
  pure (disp, Settings {setDataSets = ds})

defaultWolfDir :: MonadIO m => m (Path Abs Dir)
defaultWolfDir = (</> $(mkRelDir ".wolf")) <$> liftIO getHomeDir

deriveDataSettings :: MonadIO m => DataFlags -> m DataSettings
deriveDataSettings DataFlags {..} = do
  wd <-
    liftIO $
    case dataFlagWolfDir of
      Nothing -> defaultWolfDir
      Just d -> resolveDir' d
  pure DataSettings {dataSetWolfDir = wd, dataSetGitExecutable = dataFlagGitExecutable}

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
  args <- getArgs
  env <- getParserEnv
  let result = runArgumentsParser env args
  handleParseResult result

getParserEnv :: IO ParserEnv
getParserEnv = do
  wd <- defaultWolfDir
  i <-
    runReaderT
      getIndexWithDefault
      DataSettings {dataSetWolfDir = wd, dataSetGitExecutable = Nothing}
  -- TODO possibly do something with the information that the index does not exist yet.
  pure ParserEnv {parserEnvDefaultWolfDir = wd, parserEnvIndex = i}

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
  ReaderT $ \env -> info (helper <*> runReaderT parseArgs env) (fullDesc <> progDesc "Wolf")

-- info :: Parser a -> InfoMod a -> ParserInfo a
-- f :: ReaderT ParserEnv Parser a -> InfoMod a -> ReaderT ParserEnv ParserInfo a
parseArgs :: ReaderT ParserEnv Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: ReaderT ParserEnv Parser Command
parseCommand =
  ReaderT $ \env ->
    hsubparser $
    mconcat
      [ command "init" parseCommandInit
      , command "note" $ runReaderT parseCommandNote env
      , command "summary" $ runReaderT parseCommandSummary env
      , command "entry" $ runReaderT parseCommandEntry env
      , command "git" parseCommandGit
      , command "alias" $ runReaderT parseCommandAlias env
      , command "review" parseCommandReview
      , command "random" parseCommandRandomPerson
      , command "suggestion" parseCommandSuggestion
      , command "export" parseCommandExport
      , command "cleanup" parseCommandCleanup
      ]

parseCommandInit :: ParserInfo Command
parseCommandInit =
  let parser = pure CommandInit
      modifier = fullDesc <> progDesc "Initialise the wolf data repository."
   in info parser modifier

parseCommandNote :: ReaderT ParserEnv ParserInfo Command
parseCommandNote =
  ReaderT $ \env ->
    let parser =
          CommandNote <$>
          argument
            ((Just . T.pack) <$> str)
            (mconcat
               [ metavar "PERSON"
               , value Nothing
               , help "The first person to make a note about."
               , completeWith $ peopleMap env
               ]) <*>
          many
            (option
               (T.pack <$> str)
               (mconcat
                  [ short 'p'
                  , long "person"
                  , metavar "PERSON"
                  , help "The rest of the people to make a note about."
                  , completeWith $ peopleMap env
                  ]))
        modifier = fullDesc <> progDesc "Make a note."
     in info parser modifier

parseCommandSummary :: ReaderT ParserEnv ParserInfo Command
parseCommandSummary =
  ReaderT $ \env ->
    let parser =
          CommandSummary <$>
          argument
            (T.pack <$> str)
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
          argument
            (T.pack <$> str)
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
      modifier = fullDesc <> progDesc "Perform a git command on the wolf data."
   in info parser modifier

parseCommandAlias :: ReaderT ParserEnv ParserInfo Command
parseCommandAlias =
  ReaderT $ \env ->
    let parser =
          CommandAlias <$> argument (T.pack <$> str) (mconcat [metavar "NEW", help "The alias"]) <*>
          argument
            (T.pack <$> str)
            (mconcat
               [ metavar "OLD"
               , help "What the alias will refer to"
               , completer $ listCompleter $ peopleMap env
               ])
        modifier = fullDesc <> progDesc "Alias one identifier to an other identifier."
     in info parser modifier

parseCommandReview :: ParserInfo Command
parseCommandReview =
  let parser = CommandReview <$> periodDescriptionParser
      modifier = fullDesc <> progDesc "Review notes."
   in info parser modifier
  where
    periodDescriptionParser =
      optional $
      flag' LastDay (mconcat [long "last-day"]) <|> flag' LastWeek (mconcat [long "last-week"]) <|>
      flag' LastMonth (mconcat [long "last-month"])

parseCommandRandomPerson :: ParserInfo Command
parseCommandRandomPerson =
  let parser = pure CommandRandomPerson
      modifier = fullDesc <> progDesc "Summarise a random person."
   in info parser modifier

parseCommandSuggestion :: ParserInfo Command
parseCommandSuggestion =
  let parser =
        fmap CommandSuggestion $
        hsubparser $
        mconcat
          [command "list" parseCommandSuggestionList, command "review" parseCommandSuggestionReview]
      modifier = fullDesc <> progDesc "Manipulate Suggestions."
   in info parser modifier

parseCommandSuggestionList :: ParserInfo SuggestionFlags
parseCommandSuggestionList =
  let parser = pure CommandListSuggestions
      modifier = fullDesc <> progDesc "List all suggestions."
   in info parser modifier

parseCommandSuggestionReview :: ParserInfo SuggestionFlags
parseCommandSuggestionReview =
  let parser = pure CommandReviewSuggestion
      modifier = fullDesc <> progDesc "Review the next suggestion."
   in info parser modifier

parseCommandExport :: ParserInfo Command
parseCommandExport =
  let parser = pure CommandExport
      modifier = fullDesc <> progDesc "Export a wolf repository."
   in info parser modifier

parseCommandCleanup :: ParserInfo Command
parseCommandCleanup =
  let parser = pure CommandCleanup
      modifier = fullDesc <> progDesc "Clean up a wolf repository."
   in info parser modifier

peopleMap :: ParserEnv -> [String]
peopleMap = map (escapeSpaces . aliasString . fst) . M.toList . indexMap . parserEnvIndex
  where
    escapeSpaces = concatMap go
      where
        go ' ' = "\\ "
        go c = [c]

parseFlags :: ReaderT ParserEnv Parser Flags
parseFlags = Flags <$> parseDataFlags

parseDataFlags :: ReaderT ParserEnv Parser DataFlags
parseDataFlags = ReaderT $ \env -> parseDataFlagsWithDefault $ Just $ parserEnvDefaultWolfDir env

parseDataFlags' :: Parser DataFlags
parseDataFlags' = parseDataFlagsWithDefault Nothing

parseDataFlagsWithDefault :: Maybe (Path Abs Dir) -> Parser DataFlags
parseDataFlagsWithDefault mDefDir =
  DataFlags <$>
  option
    (Just <$> str)
    (mconcat
       [ long "wolf-dir"
       , metavar "DIR"
       , help "the data directory for all wolf data"
       , value $ toFilePath <$> mDefDir
       , case mDefDir of
           Nothing -> mempty
           Just defDir -> showDefaultWith (const $ toFilePath defDir)
       ]) <*>
  option
    (Just <$> str)
    (mconcat
       [long "git-executable", metavar "FILE", help "the git executable to use", value Nothing])
