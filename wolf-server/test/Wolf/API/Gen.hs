{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.API.Gen where

import TestImport

import qualified Data.Text as T

import Wolf.API

import Wolf.Data.People.Types.Gen ()
import Wolf.Data.Types.Gen ()

instance GenUnchecked AccountUUID

instance GenValid AccountUUID

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

instance GenValid Account

instance GenUnchecked Register

instance GenValid Register

instance GenUnchecked SetPersonAlias

instance GenValid SetPersonAlias

instance GenUnchecked PersonQuery

instance GenValid PersonQuery
