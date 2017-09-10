{-# LANGUAGE TypeApplications #-}

module Wolf.Data.People.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data

import Wolf.Data.People.Types.Gen ()

spec :: Spec
spec = do
    eqSpec @PersonUuid
    genValidSpec @PersonUuid
    jsonSpec @PersonUuid
