{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Export.TypesSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Export.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
    eqSpec @Repo
    genValidSpec @Repo
    jsonSpecOnValid @Repo
    eqSpec @ExportProblem
    genValidSpec @ExportProblem
    eqSpec @ExportWarning
    genValidSpec @ExportWarning
    monoidSpecOnValid @ExportWarning
    eqSpec @ExportError
    genValidSpec @ExportError
