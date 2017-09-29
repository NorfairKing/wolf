{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.Person.NewNote where

import Import

import Yesod
import Yesod.Form

import Wolf.Web.Server.Foundation

import Wolf.Data

getPersonNewNoteR :: PersonUuid -> Handler Html
getPersonNewNoteR uuid =
    defaultLayout $ withNavBar $(widgetFile "person/new-note")

newNoteForm :: FormInput Handler Textarea
newNoteForm = ireq textareaField "contents"

postPersonNewNoteR :: PersonUuid -> Handler Html
postPersonNewNoteR uuid = do
    Textarea contents <- runInputGet newNoteForm
    liftIO $ print contents
    redirect $ PersonR uuid
