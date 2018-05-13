{-# LANGUAGE DeriveGeneric #-}

module Wolf.Email.Suggest.Types where

import Import

data EmailNameSuggestion = EmailNameSuggestion
    { emailSuggestionName :: Text
    , emailSuggestionEmailAddress :: Text
    } deriving (Show, Eq, Generic)
