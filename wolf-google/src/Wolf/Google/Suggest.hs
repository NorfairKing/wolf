{-# LANGUAGE FlexibleContexts #-}

module Wolf.Google.Suggest where

import Import

import Control.Monad.Reader
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy.Char8 as LB8
import Data.Time
import Lens.Micro

import Network.Google.People

import Wolf.Data

import Wolf.Google.Suggest.Call
import Wolf.Google.Suggest.Gather
import Wolf.Google.Suggest.MakeSuggestion
import Wolf.Google.Suggest.Types

suggest :: (MonadIO m, MonadReader DataSettings m) => m ()
suggest = do
    resp <- liftIO getPeople
    now <- liftIO getCurrentTime
    let gatheredPeople = map gatherData $ resp ^. lcrConnections
    contexts <- getPeopleContexts
    let newSugs = mapMaybe (makeSuggestions now contexts) gatheredPeople
    liftIO $ forM_ newSugs $ LB8.putStrLn . JSON.encodePretty
    -- addPersonEntrySuggestions newSugs

getPeopleContexts ::
       (MonadIO m, MonadReader DataSettings m) => m [PersonContext]
getPeopleContexts = do
    people <- getPersonUuids
    index <- getIndexWithDefault
    forM people $ \uuid -> do
        mpe <- getPersonEntry uuid
        let aliases = map fst $ filter ((== uuid) . snd) $ indexTuples index
        pure
            PersonContext
            { personContextUuid = uuid
            , personContextAliases = aliases
            , personContextEntry = mpe
            }
