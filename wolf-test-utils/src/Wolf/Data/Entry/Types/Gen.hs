{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Entry.Types.Gen where

import Import

import Wolf.Data.Entry.Types

instance GenUnchecked PersonEntry

instance GenValid PersonEntry

instance GenUnchecked PersonPropertyValue

instance GenValid PersonPropertyValue
