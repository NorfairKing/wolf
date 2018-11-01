{-# LANGUAGE FlexibleContexts #-}

module Wolf.Data.ExportSpec
    ( spec
    ) where

import TestImport

import Wolf.Data
import Wolf.Data.Export.Types
import Wolf.Data.Gen ()
import Wolf.Data.NoteIndex.Types

import Wolf.Data.TestUtils

import Cautious.CautiousT

import qualified Data.Map as M
import qualified Data.Set as S

spec :: Spec
spec =
    withDataSetsGen $
    modifyMaxSize (`quot` 10) $
    modifyMaxSuccess (`quot` 10) $
    describe "export" $ do
        it "exports 'Nothing' if no wolf repository has been initialised" $ \gen ->
            forAll gen $ \sets -> do
                e <-
                    flip runReaderT sets $ do
                        ensureClearRepository
                        runCautiousT exportRepo
                e `shouldBe` CautiousError NoInitFile
        let roundtrip func name =
                it ("roundtrips the " ++ name) $ \gen ->
                    forAll gen $ \sets ->
                        forAllValid $ \repo -> do
                            repo' <-
                                runData sets $ do
                                    ensureClearRepository
                                    importRepo repo
                                    runCautiousT exportRepo
                            (func <$> repo') `shouldBe`
                                CautiousWarning mempty (func repo)
        -- Re-activate these if necessary for debugging
        -- roundtrip repoInitData "init data"
        -- roundtrip repoPersonIndex "person index"
        -- roundtrip repoNoteIndex "note index"
        -- roundtrip repoNoteIndices "note indices"
        -- roundtrip repoNotes "notes"
        -- roundtrip repoSuggestions "suggestions"
        roundtrip id "entire repo"
        it "fixes entry for non-existent personUuid" $ \gen ->
            forAll gen $ \sets ->
                forAllValid $ \(pu, entry) ->
                    uncheckedRepoTest sets $ \repo ->
                        repo
                            { repoPersonEntries =
                                  M.insert pu entry $ repoPersonEntries repo
                            }
        it "fixes when noteIndex contains non-existent notes" $ \gen ->
            forAll gen $ \sets ->
                forAllValid $ \nu ->
                    uncheckedRepoTest sets $ \repo -> addNoteToIndexRepo nu repo
        it "fixes when noteIndex misses notes" $ \gen ->
            forAll gen $ \sets ->
                forAllValid $ \nu ->
                    forAllValid $ \note ->
                        uncheckedRepoTest sets $ \repo ->
                            repo {repoNotes = M.insert nu note $ repoNotes repo}
        it "fixes when person mentions note but not vice versa" $ \gen ->
            forAll gen $ \sets ->
                forAllValid $ \pu ->
                    forAllValid $ \nu ->
                        uncheckedRepoTest sets $
                        addNoteToIndexRepo nu .
                        addPerson pu (NoteIndex $ S.singleton nu)
        it "fixes when note mentions person but not vice versa" $ \gen ->
            forAll gen $ \sets ->
                forAllValid $ \nu ->
                    forAllValid $ \pu ->
                        forAllValid $ \content ->
                            forAllValid $ \time ->
                                uncheckedRepoTest sets $
                                addNoteToIndexRepo nu .
                                addNote nu (Note content time $ S.singleton pu) .
                                addPerson pu (NoteIndex S.empty)
        it "fixes when person mentions non-existent note" $ \gen ->
            forAll gen $ \sets ->
                forAllValid $ \nu ->
                    forAllValid $ \pu ->
                        uncheckedRepoTest sets $
                        addPerson pu $ NoteIndex $ S.singleton nu

addNoteToIndex :: NoteUuid -> NoteIndex -> NoteIndex
addNoteToIndex nu = NoteIndex . S.insert nu . noteIndexSet

addNoteToIndexRepo :: NoteUuid -> Repo -> Repo
addNoteToIndexRepo nu repo =
    repo {repoNoteIndex = addNoteToIndex nu $ repoNoteIndex repo}

addPerson :: PersonUuid -> NoteIndex -> Repo -> Repo
addPerson pu ni repo =
    repo {repoNoteIndices = M.insert pu ni $ repoNoteIndices repo}

addNote :: NoteUuid -> Note -> Repo -> Repo
addNote nu note repo = repo {repoNotes = M.insert nu note $ repoNotes repo}

uncheckedRepoTest :: DataSettings -> (Repo -> Repo) -> Property
uncheckedRepoTest sets changeRepo =
    forAllValid $ \repo -> do
        repo' <-
            runData sets $ do
                ensureClearRepository
                importRepo $ changeRepo repo
                runCautiousT exportRepo
        case repo' of
            CautiousError e@(ExportErrorRepoInvalid _) ->
                expectationFailure $
                "Failed with the following error message:\n" ++
                prettyShowExportError e
            _ -> pure ()
