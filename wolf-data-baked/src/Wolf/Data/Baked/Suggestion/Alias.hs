{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Baked.Suggestion.Alias where

import Import

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

import Wolf.Data

import Wolf.Data.Baked.FromProperty
import Wolf.Data.Baked.Name

makeAliasSuggestions :: (MonadIO m, MonadReader DataSettings m) => m ()
makeAliasSuggestions = do
    ix <- getIndexWithDefault
    now <- liftIO getCurrentTime
    sugs <-
        forM (M.toList $ reverseIndex ix) $ \(puuid, as) -> do
            mpe <- getPersonEntry puuid
            case mpe of
                Nothing -> pure [] -- Don't suggest anything
                Just pe -> do
                    let msa = suggestedAliasFor pe
                    case msa of
                        Nothing -> pure []
                        Just sa ->
                            if S.member sa as
                                then pure []
                                else do
                                    let asug =
                                            AliasSuggestion
                                                { aliasSuggestionPerson = puuid
                                                , aliasSuggestionAlias = sa
                                                }
                                    let sas = finaliseAliasSuggestion asug now
                                    pure [sas]
    addUnusedSuggestions aliasSuggestionType $ S.fromList $ concat sugs

finaliseAliasSuggestion ::
       AliasSuggestion -> UTCTime -> Suggestion AliasSuggestion
finaliseAliasSuggestion as t =
    Suggestion
        { suggestionSuggestor = "Alias suggestor"
        , suggestionReason =
              "The entry contained enough information to suggest this alias."
        , suggestionTimestamp = t
        , suggestionData = as
        }

suggestedAliasFor :: PersonEntry -> Maybe Alias
suggestedAliasFor pe = alias <$> renderName (fromEntry pe)

completeAliasSuggestion ::
       (MonadIO m, MonadReader DataSettings m)
    => SuggestionUuid
    -> Agreement
    -> m ()
completeAliasSuggestion uuid agreement = do
    liftIO $ print agreement
    msug <- readSuggestion aliasSuggestionType uuid
    case msug of
        Nothing -> pure () -- Fine, do nothing then
        Just sug -> do
            when (agreement == Agree) $ do
                ix <- getIndexWithDefault
                let AliasSuggestion {..} = suggestionData sug
                let mix' =
                        addAlias aliasSuggestionAlias aliasSuggestionPerson ix
                case mix' of
                    Nothing -> pure ()
                    Just ix' -> putIndex ix'
            recordUsedSuggestion aliasSuggestionType sug
