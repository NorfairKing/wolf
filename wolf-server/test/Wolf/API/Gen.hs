module Wolf.API.Gen where

import TestImport

import Wolf.API

import Wolf.Data.Types.Gen

instance GenUnchecked AccountUUID

instance GenValid AccountUUID

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
