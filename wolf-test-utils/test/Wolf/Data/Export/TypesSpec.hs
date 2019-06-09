{-# LANGUAGE TypeApplications #-}

module Wolf.Data.Export.TypesSpec
  ( spec
  ) where

import TestImport

import Wolf.Data.Export.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  eqSpecOnValid @Repo
  genValidSpec @Repo
  jsonSpecOnValid @Repo
  eqSpecOnValid @ExportProblem
  genValidSpec @ExportProblem
  eqSpecOnValid @ExportWarning
  genValidSpec @ExportWarning
  monoidSpecOnValid @ExportWarning
  eqSpecOnValid @ExportError
  genValidSpec @ExportError
