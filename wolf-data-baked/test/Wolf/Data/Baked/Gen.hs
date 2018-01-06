{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Baked.Gen where

import TestImport

import Wolf.Data.Baked

instance GenUnchecked Met

instance GenValid Met where
    genValid = Met <$> genValid

instance GenUnchecked Gender

instance GenValid Gender where
    genValid =
        frequency [(20, pure Male), (20, pure Female), (1, Other <$> genValid)]

instance GenUnchecked Name

instance GenValid Name where
    genValid =
        Name <$> genValid <*> genValid <*> genValid <*> genValid <*> genValid <*>
        genValid
