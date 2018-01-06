{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Web.Server.Handler.Person where

import Import

import Data.Ord
import qualified Data.Set as S
import Data.Time

import Yesod

import Wolf.Data
import Wolf.Data.Time

import Wolf.Web.Server.Foundation

getPersonR :: PersonUuid -> Handler Html
getPersonR uuid = do
    (ix, mpe, ns) <-
        runData $ do
            ix <- getIndexWithDefault
            mpe <- getPersonEntry uuid
            ns <- sortOn (Down . noteTimestamp) <$> getPersonNotes uuid
            pure (ix, mpe, ns)
    case (reverseIndexLookupSingleAlias uuid ix , mpe, ns) of
        (Nothing, Nothing, []) -> notFound
        (malias, _, _) -> do
            now <- liftIO getCurrentTime
            token <- genToken
            let titleAlias = maybe (uuidText uuid) aliasText malias
            let placeholderAlias = maybe "..." aliasText malias
            withNavBar $(widgetFile "person")

noteWidget :: UTCTime -> Index -> PersonUuid -> Note -> Widget
noteWidget now ix uuid n = $(widgetFile "note")
  where
    otherRelevantPeople = filter (/= uuid) $ S.toList $ noteRelevantPeople n
