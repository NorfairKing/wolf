{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Suggestion
    ( Suggestion(..)
    , readPersonEntrySuggestions
    , writePersonEntrySuggestions
    , addPersonEntrySuggestions
    , readUsedPersonEntrySuggestions
    , writeUsedPersonEntrySuggestions
    , recordUsedPersonEntrySuggestions
    ) where

import Import

import Wolf.Data.Entry.Types
import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

import Wolf.Data.Suggestion.Types

suggestionsDir :: MonadReader DataSettings m => m (Path Abs Dir)
suggestionsDir = (</> $(mkRelDir "suggestions")) <$> wolfDir

entrySuggestionsFile :: MonadReader DataSettings m => m (Path Abs File)
entrySuggestionsFile = (</> $(mkRelFile "entry")) <$> suggestionsDir

usedEntrySuggestionFile :: MonadReader DataSettings m => m (Path Abs File)
usedEntrySuggestionFile = (</> $(mkRelFile "entry-used")) <$> suggestionsDir

readPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m) => m [Suggestion PersonEntry]
readPersonEntrySuggestions = entrySuggestionsFile >>= readJSONWithDefault []

writePersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion PersonEntry]
    -> m ()
writePersonEntrySuggestions ess = do
    f <- entrySuggestionsFile
    writeJSON f ess

addPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion PersonEntry]
    -> m ()
addPersonEntrySuggestions newSugs = do
    sugs <- readPersonEntrySuggestions
    let sugs' = nub $ sugs ++ newSugs
    writePersonEntrySuggestions sugs'

readUsedPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m) => m [Suggestion PersonEntry]
readUsedPersonEntrySuggestions =
    usedEntrySuggestionFile >>= readJSONWithDefault []

writeUsedPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion PersonEntry]
    -> m ()
writeUsedPersonEntrySuggestions ess = do
    f <- usedEntrySuggestionFile
    writeJSON f ess

recordUsedPersonEntrySuggestions ::
       (MonadIO m, MonadReader DataSettings m)
    => [Suggestion PersonEntry]
    -> m ()
recordUsedPersonEntrySuggestions usedSugs = do
    sugs <- readUsedPersonEntrySuggestions
    let sugs' = nub $ sugs ++ usedSugs
    writeUsedPersonEntrySuggestions sugs'
