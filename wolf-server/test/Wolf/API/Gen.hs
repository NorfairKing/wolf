{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.API.Gen where

import TestImport

import qualified Data.Text as T

import Wolf.API

import Wolf.Data.Gen ()

instance GenValid Username where
  genValid =
    sized $ \n -> do
      t <- resize n $ T.pack <$> genListOf (genValid `suchThat` validUsernameChar)
      case username t of
        Nothing -> resize n genValid
        Just un -> pure un
  shrinkValid = shrinkValidStructurally

instance GenValid PasswordHash where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Account where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid Register where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid SetPersonAlias where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally

instance GenValid PersonQuery where
  genValid = genValidStructurally
  shrinkValid = shrinkValidStructurally
