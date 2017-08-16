{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Wolf.Server.PropertiesSpec
    ( spec
    ) where

import TestImport

import Wolf.Server.TestUtils

spec :: Spec
spec =
    withWolfServer $
    describe "wolf server" $ it "starts up nicely" $ \_ -> pure () :: IO ()
