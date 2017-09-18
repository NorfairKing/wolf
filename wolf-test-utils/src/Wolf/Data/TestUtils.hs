{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.TestUtils
    ( runData
    , withDataSetsGen
    , ensureClearRepository
    , setupRepo
    , assertRepoValid
    ) where

import Import

import Wolf.Data

import Wolf.Data.Gen ()

runData :: Monad m => DataSettings -> ReaderT DataSettings m a -> m a
runData = flip runReaderT

withDataSetsGen :: SpecWith (Gen DataSettings) -> Spec
withDataSetsGen = beforeAll mkGen . afterAll_ cleanup
  where
    resolveTestSandbox = resolveDir' "test-sandbox"
    mkGen = do
        sbd <- resolveTestSandbox
        pure $ do
            rd <- genValid
            pure DataSettings {dataSetWolfDir = sbd </> rd}
    cleanup = do
        sbd <- resolveTestSandbox
        ignoringAbsence $ removeDirRecur sbd

ensureClearRepository :: (MonadIO m, MonadReader DataSettings m) => m ()
ensureClearRepository = do
    dd <- asks dataSetWolfDir
    liftIO $ ignoringAbsence $ removeDirRecur dd

setupRepo :: (MonadIO m, MonadReader DataSettings m) => Export -> m ()
setupRepo = importRepo -- TODO remove this

assertRepoValid :: DataSettings -> IO ()
assertRepoValid sets = do
    e <- runData sets exportRepo
    e `shouldSatisfy` isValid
