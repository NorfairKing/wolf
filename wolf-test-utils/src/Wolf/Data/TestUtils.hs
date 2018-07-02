{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.TestUtils
    ( runData
    , withDataSetsGen
    , ensureClearRepository
    , assertRepoValid
    , forAllSets
    ) where

import Import

import Wolf.Data

import Wolf.Data.Gen ()

import Cautious.CautiousT

runData :: Monad m => DataSettings -> ReaderT DataSettings m a -> m a
runData = flip runReaderT

withDataSetsGen :: SpecWith (Gen DataSettings) -> Spec
withDataSetsGen = beforeAll mkGen
  where
    resolveTestSandbox = resolveDir' "/tmp/test-sandbox"
    mkGen = do
        sbd <- resolveTestSandbox
        pure $ do
            rd <- genValid
            pure DataSettings {dataSetWolfDir = sbd </> rd}

ensureClearRepository :: (MonadIO m, MonadReader DataSettings m) => m ()
ensureClearRepository = do
    dd <- asks dataSetWolfDir
    liftIO $ ignoringAbsence $ removeDirRecur dd

assertRepoValid :: DataSettings -> IO ()
assertRepoValid sets = do
    mr <- runData sets $ runCautiousT exportRepo
    case mr of
        CautiousError r -> expectationFailure $ prettyShowExportError r
        CautiousWarning w _ -> w `shouldBe` mempty

forAllSets :: Testable t => (DataSettings -> t) -> Gen DataSettings -> Property
forAllSets func gen = forAll gen func
