{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Data.Suggestion
    ( Suggestion(..)
    , SuggestionType(..)
    , parseSuggestionType
    , SuggestionUuid
    , aliasSuggestionType
    , AliasSuggestion(..)
    , entrySuggestionType
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    , hashSuggestion
    , SuggestionIndex
    , emptySuggestionIndex
    , suggestionIndexMap
    , readSuggestion
    , readSuggestionChecked
    , readUnusedSuggestionIndex
    , readUnusedSuggestions
    , writeUnusedSuggestions
    , addUnusedSuggestions
    , addUnusedSuggestion
    , readUsedSuggestionIndex
    , readUsedSuggestions
    , writeUsedSuggestions
    , recordUsedSuggestions
    , recordUsedSuggestion
    , recordUsed
    , SuggestionRepo
    , SuggestionTypeRepo
    , readAllSuggestions
    , writeAllSuggestions
    ) where

import Import

import Data.Aeson as JSON
import qualified Data.Map as M
import qualified Data.Set as S

import Wolf.Data.JSONUtils
import Wolf.Data.Path
import Wolf.Data.Types

import Wolf.Data.Suggestion.Types

suggestionsDir :: MonadReader DataSettings m => m (Path Abs Dir)
suggestionsDir = (</> $(mkRelDir "suggestions")) <$> wolfDir

aliasSuggestionType :: SuggestionType
aliasSuggestionType = SuggestionType $(mkRelDir "alias")

entrySuggestionType :: SuggestionType
entrySuggestionType = SuggestionType $(mkRelDir "entry")

suggestionTypeDir ::
       MonadReader DataSettings m => SuggestionType -> m (Path Abs Dir)
suggestionTypeDir (SuggestionType d) = (</> d) <$> suggestionsDir

unusedSuggestionsIndexFile ::
       MonadReader DataSettings m => SuggestionType -> m (Path Abs File)
unusedSuggestionsIndexFile typ =
    (</> $(mkRelFile "unused.json")) <$> suggestionTypeDir typ

usedSuggestionsIndexFile ::
       MonadReader DataSettings m => SuggestionType -> m (Path Abs File)
usedSuggestionsIndexFile typ =
    (</> $(mkRelFile "used.json")) <$> suggestionTypeDir typ

emptySuggestionIndex :: SuggestionIndex a
emptySuggestionIndex = SuggestionIndex {suggestionIndexMap = M.empty}

suggestionFilePath ::
       (MonadIO m, MonadReader DataSettings m)
    => SuggestionType
    -> SuggestionUuid
    -> m (Path Abs File)
suggestionFilePath typ u = do
    d <- suggestionTypeDir typ
    resolveFile d $ uuidString u

readSuggestion ::
       forall a m.
       (MonadIO m, MonadReader DataSettings m, FromJSON a, Hashable a)
    => SuggestionType
    -> SuggestionUuid
    -> m (Maybe (Suggestion a))
readSuggestion typ u = do
    f <- suggestionFilePath typ u
    readJSONWithMaybe f

readSuggestionChecked ::
       (MonadIO m, MonadReader DataSettings m, FromJSON a, Hashable a)
    => SuggestionType
    -> SuggestionUuid
    -> SuggestionHash a
    -> m (Maybe (Suggestion a))
readSuggestionChecked typ u h = do
    ms <- readSuggestion typ u
    pure $
        (ms >>=) $ \s ->
            if hashSuggestion s == h
                then Just s
                else Nothing

writeSuggestion ::
       (MonadIO m, MonadReader DataSettings m, ToJSON a)
    => SuggestionType
    -> SuggestionUuid
    -> Suggestion a
    -> m ()
writeSuggestion typ u s = do
    f <- suggestionFilePath typ u
    writeJSON f s

readUnusedSuggestionIndex ::
       forall a m. (MonadIO m, MonadReader DataSettings m)
    => SuggestionType
    -> m (SuggestionIndex a)
readUnusedSuggestionIndex typ =
    unusedSuggestionsIndexFile typ >>= readJSONWithDefault emptySuggestionIndex

readUnusedSuggestions ::
       forall a m.
       (MonadIO m, MonadReader DataSettings m, FromJSON a, Ord a, Hashable a)
    => SuggestionType
    -> m (Map SuggestionUuid (Suggestion a))
readUnusedSuggestions = readHelper readUnusedSuggestionIndex

readHelper ::
       (MonadIO m, MonadReader DataSettings m, FromJSON a, Ord a, Hashable a)
    => (SuggestionType -> m (SuggestionIndex a))
    -> SuggestionType
    -> m (Map SuggestionUuid (Suggestion a))
readHelper rfunc typ = do
    (SuggestionIndex ix) <- rfunc typ
    ix_ <-
        M.traverseWithKey
            (\h u -> fmap ((,) u) <$> readSuggestionChecked typ u h)
            ix
    pure $ M.fromList . catMaybes . M.elems $ ix_

writeUnusedSuggestionIndex ::
       (MonadIO m, MonadReader DataSettings m)
    => SuggestionType
    -> SuggestionIndex a
    -> m ()
writeUnusedSuggestionIndex typ ix = do
    f <- unusedSuggestionsIndexFile typ
    writeJSON f ix

writeUnusedSuggestions ::
       forall a m. (MonadIO m, MonadReader DataSettings m, ToJSON a, Hashable a)
    => SuggestionType
    -> Set (Suggestion a)
    -> m ()
writeUnusedSuggestions typ ess = do
    tups <-
        forM (S.toList ess) $ \s ->
            (,) s <$> ((,) (hashSuggestion s) <$> nextRandomUUID)
    let ix = SuggestionIndex $ M.fromList $ map snd tups
    writeUnusedSuggestionIndex typ ix
    forM_ tups $ \(s, (_, u)) -> writeSuggestion typ u s

addUnusedSuggestions ::
       forall a m.
       ( MonadIO m
       , MonadReader DataSettings m
       , Eq a
       , FromJSON a
       , ToJSON a
       , Hashable a
       )
    => SuggestionType
    -> Set (Suggestion a)
    -> m ()
addUnusedSuggestions typ newSugs = do
    (SuggestionIndex ix) <- readUnusedSuggestionIndex typ
    newIx <- foldM go ix newSugs
    writeUnusedSuggestionIndex typ $ SuggestionIndex newIx
  where
    go :: Map (SuggestionHash a) SuggestionUuid
       -> Suggestion a
       -> m (Map (SuggestionHash a) SuggestionUuid)
    go m s =
        let h = hashSuggestion s
        in case M.lookup h m of
               Nothing -> do
                   u <- nextRandomUUID
                   writeSuggestion typ u s
                   pure $ M.insert h u m
               Just _ -> pure m

addUnusedSuggestion ::
       forall a m.
       ( MonadIO m
       , MonadReader DataSettings m
       , Eq a
       , FromJSON a
       , ToJSON a
       , Hashable a
       )
    => SuggestionType
    -> Suggestion a
    -> m ()
addUnusedSuggestion typ s = addUnusedSuggestions typ $ S.singleton s

readUsedSuggestionIndex ::
       forall a m. (MonadIO m, MonadReader DataSettings m)
    => SuggestionType
    -> m (SuggestionIndex a)
readUsedSuggestionIndex typ =
    usedSuggestionsIndexFile typ >>= readJSONWithDefault emptySuggestionIndex

readUsedSuggestions ::
       forall a m.
       (MonadIO m, MonadReader DataSettings m, FromJSON a, Ord a, Hashable a)
    => SuggestionType
    -> m (Map SuggestionUuid (Suggestion a))
readUsedSuggestions = readHelper readUsedSuggestionIndex

writeUsedSuggestionIndex ::
       forall a m. (MonadIO m, MonadReader DataSettings m)
    => SuggestionType
    -> SuggestionIndex a
    -> m ()
writeUsedSuggestionIndex typ ix = do
    f <- usedSuggestionsIndexFile typ
    writeJSON f ix

writeUsedSuggestions ::
       forall a m. (MonadIO m, MonadReader DataSettings m, ToJSON a, Hashable a)
    => SuggestionType
    -> Set (Suggestion a)
    -> m ()
writeUsedSuggestions typ ess = do
    tups <-
        forM (S.toList ess) $ \s ->
            (,) s <$> ((,) (hashSuggestion s) <$> nextRandomUUID)
    let ix = SuggestionIndex $ M.fromList $ map snd tups
    writeUsedSuggestionIndex typ ix
    forM_ tups $ \(s, (_, u)) -> writeSuggestion typ u s

recordUsedSuggestions ::
       forall a m.
       ( MonadIO m
       , MonadReader DataSettings m
       , Eq a
       , ToJSON a
       , FromJSON a
       , Hashable a
       )
    => SuggestionType
    -> Set (Suggestion a)
    -> m ()
recordUsedSuggestions typ usedSugs = do
    (SuggestionIndex uusi) <- readUnusedSuggestionIndex typ
    (SuggestionIndex usi) <- readUsedSuggestionIndex typ
    (uusi', usi') <- foldM (recordUsed typ) (uusi, usi) usedSugs
    writeUnusedSuggestionIndex typ $ SuggestionIndex uusi'
    writeUsedSuggestionIndex typ $ SuggestionIndex usi'

-- TODO split this into a pure and an impure function
recordUsed ::
       forall a m.
       ( MonadIO m
       , MonadReader DataSettings m
       , Eq a
       , ToJSON a
       , FromJSON a
       , Hashable a
       )
    => SuggestionType
    -> ( Map (SuggestionHash a) SuggestionUuid
       , Map (SuggestionHash a) SuggestionUuid)
    -> Suggestion a
    -> m ( Map (SuggestionHash a) SuggestionUuid
         , Map (SuggestionHash a) SuggestionUuid)
recordUsed typ (uusi, usi) s = do
    let h = hashSuggestion s
    case M.lookup h uusi of
        Nothing -> do
            uuid <- nextRandomUUID
            writeSuggestion typ uuid s
            pure (uusi, M.insert h uuid usi) -- Wasn't unused, will record it as used anyway
        Just uuid ->
            pure $
            case M.lookup h usi of
                Just uuid' ->
                    if uuid == uuid'
                        then (M.delete h uusi, usi) -- Wasn't deleted yet, let's do it now
                        else ( M.delete h uusi
                             , M.insert h uuid usi -- Wasn't deleted yet, AND had the wrong uuid.
                              )
                Nothing -> (M.delete h uusi, M.insert h uuid usi)

recordUsedSuggestion ::
       forall a m.
       ( MonadIO m
       , MonadReader DataSettings m
       , Eq a
       , ToJSON a
       , FromJSON a
       , Hashable a
       )
    => SuggestionType
    -> Suggestion a
    -> m ()
recordUsedSuggestion typ s = recordUsedSuggestions typ $ S.singleton s

readAllSuggestions ::
       (MonadIO m, MonadReader DataSettings m) => m SuggestionRepo
readAllSuggestions = do
    ssd <- suggestionsDir
    -- TODO I would prefer to use a type index instead of listing a directory
    ds <- liftIO $ fromMaybe [] <$> forgivingAbsence (fst <$> listDir ssd)
    mtups <-
        forM ds $ \ad ->
            case stripProperPrefix ssd ad of
                Nothing -> pure Nothing
                Just rd -> do
                    let typ = SuggestionType rd
                    suggestionTypeRepoUnused <-
                        readAll readUnusedSuggestionIndex typ
                    suggestionTypeRepoUsed <-
                        readAll readUsedSuggestionIndex typ
                    pure $ Just (typ, SuggestionTypeRepo {..})
    pure $ SuggestionRepo $ M.fromList $ catMaybes mtups
  where
    readAll ::
           (MonadIO m, MonadReader DataSettings m)
        => (SuggestionType -> m (SuggestionIndex JSON.Value))
        -> SuggestionType
        -> m (Map SuggestionUuid (Suggestion JSON.Value))
    readAll readFunc typ = do
        SuggestionIndex ix <- readFunc typ
        msugs <-
            forM (M.toList ix) $ \(h, uuid) -> do
                msug <- readSuggestionChecked typ uuid h
                pure $ (,) uuid <$> msug
        pure $ M.fromList $ catMaybes msugs

writeAllSuggestions ::
       (MonadIO m, MonadReader DataSettings m) => SuggestionRepo -> m ()
writeAllSuggestions (SuggestionRepo m) = do
    suggestionsDir >>= ensureDir
    void $
        flip M.traverseWithKey m $ \typ SuggestionTypeRepo {..} -> do
            suggestionTypeDir typ >>= ensureDir
            writeAll writeUnusedSuggestionIndex typ suggestionTypeRepoUnused
            writeAll writeUsedSuggestionIndex typ suggestionTypeRepoUsed
  where
    writeAll ::
           (MonadIO m, MonadReader DataSettings m)
        => (SuggestionType -> SuggestionIndex JSON.Value -> m ())
        -> SuggestionType
        -> Map SuggestionUuid (Suggestion JSON.Value)
        -> m ()
    writeAll writeFunc typ sm = do
        writeFunc typ $ makeIndex sm
        void $ M.traverseWithKey (writeSuggestion typ) sm
    makeIndex ::
           Map SuggestionUuid (Suggestion JSON.Value)
        -> SuggestionIndex JSON.Value
    makeIndex m_ =
        SuggestionIndex $
        M.fromList $ flip map (M.toList m_) $ \(u, s) -> (hashSuggestion s, u)
