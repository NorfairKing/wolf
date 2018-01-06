{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Suggestion
    ( Suggestion(..)
    , SuggestionType
    , aliasSuggestionType
    , AliasSuggestion(..)
    , entrySuggestionType
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    , readSuggestions
    , writeSuggestions
    , addSuggestions
    , readUsedSuggestions
    , writeUsedSuggestions
    , recordUsedSuggestions
    ) where

import Import

import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

import Wolf.Data.Suggestion.Types

suggestionsDir :: MonadReader DataSettings m => m (Path Abs Dir)
suggestionsDir = (</> $(mkRelDir "suggestions")) <$> wolfDir

aliasSuggestionType :: SuggestionType AliasSuggestion
aliasSuggestionType = SuggestionType $(mkRelDir "alias")

entrySuggestionType :: SuggestionType EntrySuggestion
entrySuggestionType = SuggestionType $(mkRelDir "entry")

entrySuggestionsFile ::
       MonadReader DataSettings m => SuggestionType a -> m (Path Abs File)
entrySuggestionsFile (SuggestionType rf) =
    (</> rf </> $(mkRelFile "unused.json")) <$> suggestionsDir

usedSuggestionFile ::
       MonadReader DataSettings m => SuggestionType a -> m (Path Abs File)
usedSuggestionFile (SuggestionType rf) =
    (</> rf </> $(mkRelFile "used.json")) <$> suggestionsDir

readSuggestions ::
       (MonadIO m, MonadReader DataSettings m, FromJSON a)
    => SuggestionType a
    -> m [Suggestion a]
readSuggestions typ = entrySuggestionsFile typ >>= readJSONWithDefault []

writeSuggestions ::
       (MonadIO m, MonadReader DataSettings m, ToJSON a)
    => SuggestionType a
    -> [Suggestion a]
    -> m ()
writeSuggestions typ ess = do
    f <- entrySuggestionsFile typ
    writeJSON f ess

addSuggestions ::
       (MonadIO m, MonadReader DataSettings m, Eq a, FromJSON a, ToJSON a)
    => SuggestionType a
    -> [Suggestion a]
    -> m ()
addSuggestions typ newSugs = do
    usedSugs <- readSuggestions typ
    sugs <- readSuggestions typ
    let sugs' = nub $ sugs ++ (newSugs \\ usedSugs)
    writeSuggestions typ sugs'

readUsedSuggestions ::
       (MonadIO m, MonadReader DataSettings m, FromJSON a)
    => SuggestionType a
    -> m [Suggestion a]
readUsedSuggestions typ = usedSuggestionFile typ >>= readJSONWithDefault []

writeUsedSuggestions ::
       (MonadIO m, MonadReader DataSettings m, ToJSON a)
    => SuggestionType a
    -> [Suggestion a]
    -> m ()
writeUsedSuggestions typ ess = do
    f <- usedSuggestionFile typ
    writeJSON f ess

recordUsedSuggestions ::
       (MonadIO m, MonadReader DataSettings m, Eq a, ToJSON a, FromJSON a)
    => SuggestionType a
    -> [Suggestion a]
    -> m ()
recordUsedSuggestions typ usedSugs = do
    psugs <- readSuggestions typ
    writeSuggestions typ $ psugs \\ usedSugs
    sugs <- readUsedSuggestions typ
    let sugs' = nub $ sugs ++ usedSugs
    writeUsedSuggestions typ sugs'
