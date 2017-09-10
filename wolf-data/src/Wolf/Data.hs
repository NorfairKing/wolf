module Wolf.Data
    ( DataSettings(..)
    -- * Init
    , InitData(..)
    , getInitData
    , initWolf
    , withInitCheck
    -- * Global index
    , PersonUuid
    , personUuidLBs
    , personUuidText
    , personUuidString
    , nextRandomPersonUuid
    , getPersonUuids
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
    , newPersonEntry
    , PersonProperty(..)
    , sameValues
    , PersonPropertyValue(..)
    , sameContents
    , getPersonEntry
    , putPersonEntry
    -- * Notes
    , NoteUuid
    , NoteIndex(..)
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
    ) where

import Wolf.Data.Entry
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.People
import Wolf.Data.Suggestion
import Wolf.Data.Types
