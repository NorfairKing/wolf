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
    withNavBar $(widgetFile "new-note")

data NewNote = NewNote
    { newNotePeople :: [PersonUuid]
    , newNoteContents :: Text
    }

newtype InvalidNewNote =
    InvalidPersonUuid Text

instance RenderMessage App InvalidNewNote where
    renderMessage _ _ (InvalidPersonUuid t) = "Invalid Person Uuid: " <> t

newNoteForm :: [(PersonUuid, Alias)] -> FormInput Handler NewNote
newNoteForm il =
    NewNote <$>
    ireq
        (multiSelectField $
         pure
             OptionList
                 { olOptions =
                       map
                           (\(u, a) ->
                                Option
                                    { optionDisplay = aliasText a
                                    , optionInternalValue = u
                                    , optionExternalValue = uuidText u
                                    })
                           il
                 , olReadExternal = parseUUID
                 })
        "uuid" <*>
    ireq textField "contents"

postNewNoteR :: Handler Html
postNewNoteR = do
    ix <- runData getIndexWithDefault
    let il = sortOn snd $ M.toList $ reverseIndexSingleAlias ix
    NewNote {..} <- runInputPost $ newNoteForm il
    now <- liftIO getCurrentTime
    runData $ do
        noteUuid <-
            createNewNote
                Note
                    { noteContents = newNoteContents
                    , noteTimestamp = now
                    , noteRelevantPeople = S.fromList newNotePeople
                    }
        makeGitCommit $
            unwords
                [ "Added note on"
                , intercalate ", " $
                  flip map newNotePeople $ \u ->
                      maybe (uuidString u) aliasString $
                      reverseIndexLookupSingleAlias u ix
                , "with uuid"
                , uuidString noteUuid
                , "via wolf-web-server."
                ]
    redirect HomeR
