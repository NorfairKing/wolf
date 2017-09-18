{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.TestUtils
    ( withDataSetsGen
    , ensureClearRepository
    ) where

import Import

import Wolf.Data

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
