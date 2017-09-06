{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.Suggestion
    ( Suggestion(..)
    , readPersonEntrySuggestions
    , writePersonEntrySuggestions
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
