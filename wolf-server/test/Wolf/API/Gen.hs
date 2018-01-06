{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.API.Gen where

import TestImport

import qualified Data.Text as T

import Wolf.API

import Wolf.Data.Gen ()

instance GenUnchecked Username

instance GenValid Username where
    genValid =
        sized $ \n -> do
            t <-
                resize n $
                T.pack <$> genListOf (genValid `suchThat` validUsernameChar)
            case username t of
                Nothing -> resize n genValid
                Just un -> pure un

instance GenUnchecked PasswordHash

instance GenValid PasswordHash

instance GenUnchecked Account

instance GenValid Account where
    genValid = Account <$> genValid <*> genValid <*> genValid

instance GenUnchecked Register

instance GenValid Register where
    genValid = Register <$> genValid <*> genValid

instance GenUnchecked SetPersonAlias

instance GenValid SetPersonAlias where
    genValid = SetPersonAlias <$> genValid <*> genValid

instance GenUnchecked PersonQuery

instance GenValid PersonQuery
