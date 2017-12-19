{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Wolf.Web.Server.Handler.NewNote where

import Import

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time

import Yesod

import Wolf.Web.Server.Foundation

import Wolf.Data
import Wolf.Data.Git

getNewNoteR :: Handler Html
getNewNoteR = do
    ix <- runData getIndexWithDefault
    let il = sortOn snd $ M.toList $ reverseIndexSingleAlias ix
    token <- genToken
    defaultLayout $ withNavBar $(widgetFile "new-note")

data NewNote = NewNote
    { newNotePerson :: PersonUuid
    , newNoteContents :: Text
    }

newtype InvalidNewNote =
    InvalidPersonUuid Text

instance RenderMessage App InvalidNewNote where
    renderMessage _ _ (InvalidPersonUuid t) = "Invalid Person Uuid: " <> t

newNoteForm :: FormInput Handler NewNote
newNoteForm =
    NewNote <$>
    ireq
        (checkMMap
             (\t ->
                  pure . maybe (Left $ InvalidPersonUuid t) Right $
                  parsePersonUuid t)
             personUuidText
             textField)
        "uuid" <*>
    ireq textField "contents"

postNewNoteR :: Handler Html
postNewNoteR = do
    NewNote {..} <- runInputPost newNoteForm
    now <- liftIO getCurrentTime
    runData $ do
        noteUuid <-
            createNewNote
                Note
                { noteContents = newNoteContents
                , noteTimestamp = now
                , noteRelevantPeople = S.singleton newNotePerson
                }
        makeGitCommit $
            unwords
                [ "Added note on person with uuid"
                , personUuidString newNotePerson
                , "with uuid"
                , noteUuidString noteUuid
                , "via wolf-web-server."
                ]
    redirect HomeR
