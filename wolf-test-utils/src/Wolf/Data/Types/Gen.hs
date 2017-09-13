{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Types.Gen where

import Import

import Wolf.Data.Types

instance GenUnchecked DataSettings

instance GenValid DataSettings
