module Wolf.Data
    ( DataSettings(..)
    -- * Init
    , InitData(..)
    , readInitData
    , initWolf
    , withInitCheck
    , withInitCheck_
    -- * Global index
    , PersonUuid
    , personUuidLBs
    , personUuidText
    , personUuidString
    , nextRandomPersonUuid
    , getPersonUuids
    , Alias
    , alias
    , aliasText
    , aliasString
    , Index
    , indexMap
    , indexKeys
    , indexTuples
    , lookupInIndex
    , reverseIndexLookup
    , reverseIndexLookupSingleAlias
    , addIndexEntry
    , createNewPerson
    , addAliases
    , lookupOrCreateNewPerson
    , getIndex
    , getIndexWithDefault
    , putIndex
    -- * Person Entries
    , PersonEntry
    , personEntry
    , personEntryProperties
    , sameProperties
    , newPersonEntry
    , PersonProperty(..)
    , sameValues
    , PersonPropertyValue(..)
    , sameContents
    , entryContents
    , entryContentsBS
    , updatePersonEntry
    , UpdateResult(..)
    , EntryParseException
    , prettyPrintEntryParseException
    , getPersonEntry
    , putPersonEntry
    , deletePersonEntry
    -- * Notes
    , NoteUuid
    , NoteIndex
    , newNoteIndex
    , nextRandomNoteUuid
    , noteUuidText
    , noteUuidString
    , addToNoteIndex
    , containsNoteUuid
    -- ** Global note index
    , getNoteIndex
    , putNoteIndex
    , getNoteUuids
    , getNotes
    -- ** Person note index
    , getPersonNoteIndex
    , putPersonNoteIndex
    , getPersonNoteUuids
    , getPersonNotes
    -- ** Notes
    , Note(..)
    , createNewNote
    , createNewNoteUuid
    , readNote
    , writeNote
    -- * Suggestions
    , Suggestion(..)
    , EntrySuggestion(..)
    , sameEntrySuggestionData
    , sameEntrySuggestion
    , readPersonEntrySuggestions
    , addPersonEntrySuggestions
    , readUsedPersonEntrySuggestions
    , recordUsedPersonEntrySuggestions
    -- * Export
    , Export
    , exportInitData
    , exportPersonIndex
    , export
    ) where

import Wolf.Data.Entry
import Wolf.Data.Export
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types
