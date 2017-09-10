{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Suggestion
    ( Suggestion(..)
    , EntrySuggestion(..)
    , readPersonEntrySuggestions
    , writePersonEntrySuggestions
    , addPersonEntrySuggestions
    , readUsedPersonEntrySuggestions
    , writeUsedPersonEntrySuggestions
    , recordUsedPersonEntrySuggestions
    ) where

import Import

import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

import Wolf.Data.Suggestion.Types

suggestionsDir :: MonadReader DataSettings m => m (Path Abs Dir)
suggestionsDir = (</> $(mkRelDir "suggestions")) <$> wolfDir

entrySuggestionsFile :: MonadReader DataSettings m => m (Path Abs File)
entrySuggestionsFile = (</> $(mkRelFile "entry.json")) <$> suggestionsDir

usedEntrySuggestionFile :: MonadReader DataSettings m => m (Path Abs File)
usedEntrySuggestionFile =
    (</> $(mkRelFile "entry-used.json")) <$> suggestionsDir

readPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m) => m [Suggestion EntrySuggestion]
readPersonEntrySuggestions = entrySuggestionsFile >>= readJSONWithDefault []

writePersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion EntrySuggestion]
    -> m ()
writePersonEntrySuggestions ess = do
    f <- entrySuggestionsFile
    writeJSON f ess

addPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion EntrySuggestion]
    -> m ()
addPersonEntrySuggestions newSugs = do
    sugs <- readPersonEntrySuggestions
    let sugs' = nub $ sugs ++ newSugs
    writePersonEntrySuggestions sugs'

readUsedPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m) => m [Suggestion EntrySuggestion]
readUsedPersonEntrySuggestions =
    usedEntrySuggestionFile >>= readJSONWithDefault []

writeUsedPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion EntrySuggestion]
    -> m ()
writeUsedPersonEntrySuggestions ess = do
    f <- usedEntrySuggestionFile
    writeJSON f ess

recordUsedPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion EntrySuggestion]
    -> m ()
recordUsedPersonEntrySuggestions usedSugs = do
    psugs <- readPersonEntrySuggestions
    writePersonEntrySuggestions $ psugs \\ usedSugs
    sugs <- readUsedPersonEntrySuggestions
    let sugs' = nub $ sugs ++ usedSugs
    writeUsedPersonEntrySuggestions sugs'
