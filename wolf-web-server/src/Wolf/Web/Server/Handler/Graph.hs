{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.Graph where

import Import

import qualified Data.Set as S

import Yesod

import Wolf.Data

import Wolf.Web.Server.Foundation

getGraphR :: Handler Html
getGraphR = do
    nTups <-
        runData $ do
            nuuids <- getNoteUuids
            fmap catMaybes $
                forM nuuids $ \nuuid -> do
                    mNote <- readNote nuuid
                    pure $ (,) nuuid <$> mNote
    pTups <-
        runData $ do
            ix <- getIndexWithDefault
            puuids <- getPersonUuids
            pure $
                flip map puuids $ \puuid ->
                    (,)
                        puuid
                        (maybe (uuidText puuid) aliasText $
                         reverseIndexLookupSingleAlias puuid ix)
    withNavBar $(widgetFile "graph")
