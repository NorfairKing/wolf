{-# LANGUAGE TypeApplications #-}

module Wolf.Data.BakedSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Baked

import Wolf.Data.Baked.Gen ()
import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @Met
    genValidSpec @Met
    eqSpec @Gender
    genValidSpec @Gender
    eqSpec @Name
    genValidSpec @Name
