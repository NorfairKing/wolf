{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.API.Gen where

import TestImport

import Wolf.API

import Wolf.Data.Index.Types.Gen ()
import Wolf.Data.People.Types.Gen ()
import Wolf.Data.Types.Gen ()

instance GenUnchecked SetPersonAlias

instance GenValid SetPersonAlias

instance GenUnchecked PersonQuery

instance GenValid PersonQuery
