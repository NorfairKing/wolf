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
    , Index
    , indexMap
    , indexKeys
    , indexTuples
    , lookupInIndex
    , addIndexEntry
    , lookupOrCreateNewPerson
    , getIndex
    , getIndexWithDefault
    , putIndex
    -- * Person Entries
    , PersonEntry
    , personEntry
    , personEntryProperty
    , newPersonEntry
    , PersonProperty(..)
    , WithLastChanged(..)
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
    ) where

import Wolf.Data.Entry
import Wolf.Data.Index
import Wolf.Data.Init
import Wolf.Data.Note
import Wolf.Data.NoteIndex
import Wolf.Data.Types
