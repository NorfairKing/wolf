{-# LANGUAGE TypeApplications #-}

module Wolf.Data.NoteIndex.TypesSpec
  ( spec
  ) where

import TestImport

import Wolf.Data.NoteIndex.Types

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  genValidSpec @NoteIndex
  jsonSpecOnValid @NoteIndex
  genValidSpec @NoteUuid
  jsonSpec @NoteUuid
