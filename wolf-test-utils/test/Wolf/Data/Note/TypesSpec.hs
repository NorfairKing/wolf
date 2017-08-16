{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Note.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Note.Types

import Wolf.Data.Note.Types.Gen ()

spec :: Spec
spec = do
    genValidSpec @PersonNote
    jsonSpecOnValid @PersonNote
