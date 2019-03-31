{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Wolf.Data.Baked
  ( module Wolf.Data.Baked
  , module Wolf.Data.Baked.Name
  , module Wolf.Data.Baked.FromProperty
  , module Wolf.Data.Baked.Suggestion.Alias
  ) where

import Import

import qualified Data.Text.Encoding as TE
import Text.Email.Validate as Validate (EmailAddress, emailAddress)

import Wolf.Data

import Wolf.Data.Baked.FromProperty
import Wolf.Data.Baked.Name
import Wolf.Data.Baked.Suggestion.Alias

data EmailAddressWithPurpose =
  EmailAddressWithPurpose (Maybe Text) EmailAddress
  deriving (Show, Eq, Generic)

instance FromProperty [EmailAddressWithPurpose] where
  fromProperty pp =
    case pp `atKey` "email" of
      Nothing -> []
      Just pp' -> emailsFrom pp'
    where
      emailsFrom pp' =
        case pp' of
          PVal t ->
            case Validate.emailAddress $
                 TE.encodeUtf8 $ personPropertyValueContents t of
              Nothing -> []
              Just ea -> [EmailAddressWithPurpose Nothing ea]
          PList ls -> concatMap emailsFrom ls
          PMap ls ->
            flip concatMap ls $ \(t, p) ->
              map
                (\(EmailAddressWithPurpose mp ea) ->
                   EmailAddressWithPurpose (Just $ fromMaybe t mp) ea) $
              emailsFrom p

newtype Met =
  Met
    { metText :: Text
    }
  deriving (Show, Eq, Generic)

instance Validity Met

instance FromProperty (Maybe Met) where
  fromProperty pp = Met <$> pp `atKey` "met"

data Gender
  = Male
  | Female
  | Other Text
  deriving (Show, Eq, Generic)

instance Validity Gender

instance FromProperty (Maybe Gender) where
  fromProperty pp =
    flip fmap (pp `atKey` "gender") $ \gt ->
      case gt of
        "male" -> Male
        "female" -> Female
        _ -> Other gt
