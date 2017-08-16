{-# OPTIONS_GHC -fno-warn-orphans #-}

module Wolf.Data.Types.Gen where

import Import

import Data.UUID.Types

import Wolf.Data.Types

instance Validity UUID where
    isValid = const True

instance GenUnchecked UUID where
    genUnchecked =
        let genWord = fromIntegral <$> (genUnchecked :: Gen Int)
        in fromWords <$> genWord <*> genWord <*> genWord <*> genWord

instance GenValid UUID

instance GenUnchecked InitData

instance GenValid InitData

instance GenUnchecked Index

instance GenValid Index where
    genValid = Index <$> genValid

instance GenUnchecked PersonUuid

instance GenValid PersonUuid where
    genValid = PersonUuid <$> genValid

instance GenUnchecked NoteIndex

instance GenUnchecked PersonNoteUuid
