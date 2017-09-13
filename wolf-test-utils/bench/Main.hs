{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.DeepSeq
import Data.GenValidity
import Data.Typeable
import Test.QuickCheck

import Criterion.Main as Criterion

import Wolf.Data
import Wolf.Data.Gen ()

main :: IO ()
main =
    Criterion.defaultMain
        [ genValidBench @DataSettings
        , genValidBench @InitData
        , genValidBench @PersonUuid
        , genValidBench @Alias
        , genValidBench @Index
        , genValidBench @PersonPropertyValue
        , genValidBench @PersonProperty
        , genValidBench @PersonEntry
        , genValidBench @NoteUuid
        , genValidBench @Note
        , genValidBench @(Suggestion EntrySuggestion)
        ]

genValidBench ::
       forall a. (Typeable a, GenValid a, NFData a)
    => Benchmark
genValidBench =
    bench ("genValid :: Gen " ++ show (typeRep (Proxy @a))) $
    nfIO $ generate (genValid :: Gen a)
