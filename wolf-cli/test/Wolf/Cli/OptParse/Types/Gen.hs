{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Cli.OptParse.Types.Gen where

import TestImport

import Wolf.Cli.OptParse.Types

import Wolf.Data.Gen ()

instance GenValid Command where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SuggestionFlags where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Flags where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid DataFlags where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Configuration where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Dispatch where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PeriodDescription where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SuggestionSettings where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Settings where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid ParserEnv where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
