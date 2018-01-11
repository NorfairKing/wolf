{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Baked.Suggestion.Alias where

import Import

import qualified Data.Map as M
import qualified Data.Set as S

import Wolf.Data

import Wolf.Data.Baked.FromProperty
import Wolf.Data.Baked.Name

makeAliasSuggestions :: (MonadIO m, MonadReader DataSettings m) => m ()
makeAliasSuggestions = do
    ix <- getIndexWithDefault
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
                                    let sas = finaliseAliasSuggestion asug
                                    pure [sas]
    addUnusedSuggestions aliasSuggestionType $ S.fromList $ concat sugs

finaliseAliasSuggestion :: AliasSuggestion -> Suggestion AliasSuggestion
finaliseAliasSuggestion as =
    Suggestion
    { suggestionSuggestor = "Alias suggestor"
    , suggestionReason =
          "The entry contained enough information to suggest this alias"
    , suggestionData = as
    }

suggestedAliasFor :: PersonEntry -> Maybe Alias
suggestedAliasFor pe = alias <$> renderName (fromEntry pe)
