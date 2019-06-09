{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.API.Gen where

import TestImport

import Wolf.API

import Wolf.Data.Gen ()

instance GenValid SetPersonAlias where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PersonQuery where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
