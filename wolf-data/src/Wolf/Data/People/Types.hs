module Wolf.Data.People.Types
    ( PersonUuid
    ) where

import Import

import Data.UUID.Typed

type PersonUuid = UUID Person

data Person
