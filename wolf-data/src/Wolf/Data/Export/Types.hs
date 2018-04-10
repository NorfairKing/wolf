{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Wolf.Data.Export.Types
    ( Repo(..)
    , CautiousExport
    , cautiousProblem
    , cautiousProblemM
    , cautiousProblemIfNothing
    , prettyShowExportWarning
    , prettyShowExportError
    , ExportWarning
    , ExportProblem(..)
    , ExportError(..)
    , InvalidRepoMessage
    , eitherInvalidRepoMessage
    ) where

import Import

import Data.Aeson
import qualified Data.Map as M

import qualified Data.Set as S

import Cautious.CautiousT

import Wolf.Data.Entry.Types
import Wolf.Data.Index
import Wolf.Data.Init.Types
import Wolf.Data.Note.Types
import Wolf.Data.NoteIndex.Types
import Wolf.Data.People.Types
import Wolf.Data.Suggestion.Types

data Repo = Repo
    { repoInitData :: InitData
    , repoPersonIndex :: Index
    , repoPersonEntries :: Map PersonUuid PersonEntry
    , repoNoteIndex :: NoteIndex
    , repoNoteIndices :: Map PersonUuid NoteIndex
    , repoNotes :: Map NoteUuid Note
    , repoSuggestions :: SuggestionRepo
    } deriving (Show, Eq, Generic)

instance Validity Repo where
    validate Repo {..} =
        mconcat
            [ repoInitData <?!> "repoInitData"
            , repoPersonIndex <?!> "repoPersonIndex"
            , repoPersonEntries <?!> "repoPersonEntries"
            , repoNoteIndex <?!> "repoNoteIndex"
            , repoNoteIndices <?!> "repoNoteIndices"
            , repoNotes <?!> "repoNotes"
            , M.keysSet repoNotes ==
              noteIndexSet repoNoteIndex <?@>
              "The key set of repoNotes equals the note UUID's in the global note index."
            , mconcat $
              flip map (M.toList repoNoteIndices) $ \(personUuid, noteIndex) ->
                  noteIndex `isSubNoteIndexOf` repoNoteIndex <?@>
                  unlines
                      [ "The person note index for person"
                      , uuidString personUuid
                      , "is a sub-noteindex of the global note index."
                      , "Person note index: " ++ show noteIndex
                      , "Global note index: " ++ show repoNoteIndex
                      ]
            , repoSuggestions <?!> "repoSuggestions"
            , mconcat $
              itsNotesMentionPerson repoNoteIndices repoNotes <$>
              (snd <$> indexTuples repoPersonIndex) -- If pu refers to nu, nu refers to pu
            , mconcat $
              itsPeopleMentionNote repoNoteIndices repoNotes <$>
              S.toList (noteIndexSet repoNoteIndex)
            -- If nu refers to pu, pu refers to nu
            ]
    isValid = isValidByValidating

getPersonNotes :: Map PersonUuid NoteIndex -> PersonUuid -> [NoteUuid]
getPersonNotes noteIndices pu =
    case M.lookup pu noteIndices of
        Nothing -> []
        Just ni -> S.toList $ noteIndexSet ni

getNotePeople :: Map NoteUuid Note -> NoteUuid -> [PersonUuid]
getNotePeople noteMap nu =
    case M.lookup nu noteMap of
        Nothing -> []
        Just Note {..} -> S.toList noteRelevantPeople

itsNotesMentionPerson ::
       Map PersonUuid NoteIndex -> Map NoteUuid Note -> PersonUuid -> Validation
itsNotesMentionPerson noteIndices noteMap pu =
    mconcat $ isRelevantTo pu noteMap <$> getPersonNotes noteIndices pu

itsPeopleMentionNote ::
       Map PersonUuid NoteIndex -> Map NoteUuid Note -> NoteUuid -> Validation
itsPeopleMentionNote noteIndices noteMap nu =
    mconcat $ isMentionedBy nu noteIndices <$> getNotePeople noteMap nu

isMentionedBy ::
       NoteUuid -> Map PersonUuid NoteIndex -> PersonUuid -> Validation
isMentionedBy nu noteIndices pu =
    check (elem nu $ getPersonNotes noteIndices pu) $
    mconcat
        [ "Note "
        , show nu
        , " mentions person "
        , show pu
        , ", so the person refers to the note."
        ]

isRelevantTo :: PersonUuid -> Map NoteUuid Note -> NoteUuid -> Validation
isRelevantTo pu noteMap nu =
    check (elem pu $ getNotePeople noteMap nu) $
    mconcat
        [ "Person "
        , show pu
        , " refers to note "
        , show nu
        , ", so the note refers to the person."
        ]

instance NFData Repo

instance FromJSON Repo where
    parseJSON =
        withObject "Repo" $ \o ->
            Repo <$> o .: "init-data" <*> o .: "person-index" <*>
            o .: "person-entries" <*>
            o .: "note-index" <*>
            o .: "note-indices" <*>
            o .: "notes" <*>
            o .: "suggestions"

instance ToJSON Repo where
    toJSON Repo {..} =
        object
            [ "init-data" .= repoInitData
            , "person-index" .= repoPersonIndex
            , "person-entries" .= repoPersonEntries
            , "note-index" .= repoNoteIndex
            , "note-indices" .= repoNoteIndices
            , "notes" .= repoNotes
            , "suggestions" .= repoSuggestions
            ]

type ExportWarning = [ExportProblem]

data ExportProblem
    = ExportWarningMissingNoteIndex PersonUuid
                                    NoteUuid
    | ExportWarningMissingNote NoteUuid
    | ExportWarningIncorrectNoteIndex NoteIndex
                                      NoteIndex
    | ExportWarningMissingRelevantPerson PersonUuid
                                         NoteUuid
    | ExportWarningMissingRelevantNote NoteUuid
                                       PersonUuid
    deriving (Show, Eq, Generic)

instance Validity ExportProblem

instance ToJSON ExportProblem

instance FromJSON ExportProblem

newtype InvalidRepoMessage =
    InvalidRepoMessage String
    deriving (Show, Eq, Generic)

eitherInvalidRepoMessage :: Repo -> Either InvalidRepoMessage Repo
eitherInvalidRepoMessage repo =
    case prettyValidation repo of
        Left errMess -> Left $ InvalidRepoMessage errMess
        Right r -> Right r

instance Validity InvalidRepoMessage

instance ToJSON InvalidRepoMessage

instance FromJSON InvalidRepoMessage

data ExportError
    = NoInitFile
    | ExportErrorRepoInvalid InvalidRepoMessage
    deriving (Show, Eq, Generic)

instance Validity ExportError

instance ToJSON ExportError

instance FromJSON ExportError

type CautiousExport = CautiousT ExportWarning ExportError

cautiousProblem :: Monad m => ExportProblem -> a -> CautiousExport m a
cautiousProblem = cautiousWarning . pure

cautiousProblemIfNothing ::
       Monad m => ExportProblem -> Maybe a -> CautiousExport m (Maybe a)
cautiousProblemIfNothing = cautiousWarningIfNothing . pure

cautiousProblemM :: Monad m => ExportProblem -> m a -> CautiousExport m a
cautiousProblemM = cautiousWarningM . pure

prettyShowExportError :: ExportError -> String
prettyShowExportError NoInitFile =
    "Error: the wolf repository is not or poorly initialised."
prettyShowExportError (ExportErrorRepoInvalid (InvalidRepoMessage err)) = err

prettyShowExportProblem :: ExportProblem -> String
prettyShowExportProblem (ExportWarningMissingNoteIndex pu nu) =
    mconcat
        [ "ExportWarning: "
        , show pu
        , " has no NoteIndex, but needs to have one since note "
        , show nu
        , " mentions him/her."
        ]
prettyShowExportProblem (ExportWarningMissingNote nu) =
    "ExportWarning: " ++ show nu ++ " has no note."
prettyShowExportProblem (ExportWarningMissingRelevantPerson pu nu) =
    "ExportWarning: The note " ++
    show nu ++ " does not mention the person " ++ show pu ++ "."
prettyShowExportProblem (ExportWarningMissingRelevantNote nu pu) =
    "ExportWarning: The person " ++
    show pu ++
    " does not mention the note " ++ show nu ++ " in their noteIndex."
prettyShowExportProblem (ExportWarningIncorrectNoteIndex tooMany tooFew) =
    unlines
        [ "ExportWarning: The global note index is missing these:"
        , show tooFew
        , "and has these too many:"
        , show tooMany
        ]

prettyShowExportWarning :: ExportWarning -> String
prettyShowExportWarning [] = "Everything succeeded!"
prettyShowExportWarning xs = intercalate "\n" $ prettyShowExportProblem <$> xs
