module Wolf.Data.NoteIndexSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Note
import Wolf.Data.Import
import Wolf.Data.NoteIndex

import Wolf.Data.Gen
import Wolf.Data.TestUtils

spec :: Spec
spec = do
    withDataSetsGen $ do
        describe "getNoteIndex" $
            it "retrieves the note index that was just written" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \noteIndex -> do
                        ni <-
                            flip runReaderT sets $ do
                                putNoteIndex noteIndex
                                getNoteIndex
                        ni `shouldBe` noteIndex
        describe "putNoteIndex" $
            it "does not crash" $ \gen ->
                forAll gen $ \sets ->
                    forAll genUnchecked $ \noteIndex ->
                        runReaderT (putNoteIndex noteIndex) sets `shouldReturn`
                        ()
        describe "getPersonNoteIndex" $
            it "retrieves the note index that was just written" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \personUuid ->
                        forAll genValid $ \noteIndex -> do
                            ni <-
                                flip runReaderT sets $ do
                                    putPersonNoteIndex personUuid noteIndex
                                    getPersonNoteIndex personUuid
                            ni `shouldBe` noteIndex
        describe "putPersonNoteIndex" $
            it "does not crash" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \personUuid ->
                        forAll genUnchecked $ \noteIndex ->
                            runReaderT
                                (putPersonNoteIndex personUuid noteIndex)
                                sets `shouldReturn`
                            ()
        describe "createNewNoteUuid" $ do
            it "does not crash and generates a valid noteUuid" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \note -> do
                        noteUuid <- runReaderT (createNewNote note) sets
                        noteUuid `shouldSatisfy` isValid
            it "will leave the new note uuid in the global note index" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \note -> do
                        (noteUuid, gni) <-
                            flip runReaderT sets $
                            (,) <$> createNewNote note <*> getNoteIndex
                        gni `shouldSatisfy` (`containsNoteUuid` noteUuid)
            it
                "will leave the new note uuid in the each of the relevant people's note index" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \note -> do
                        (noteUuid, pnis) <-
                            flip runReaderT sets $ do
                                nid <- createNewNote note
                                pnis <-
                                    forM (toList $ noteRelevantPeople note) $ \personUuid ->
                                        (,) personUuid <$>
                                        getPersonNoteIndex personUuid
                                pure (nid, pnis)
                        forM_ pnis $ \(_, personNoteIndex) ->
                            personNoteIndex `shouldSatisfy`
                            (`containsNoteUuid` noteUuid)
        describe "createNewNote" $
            it "leaves a valid repository valid" $ \gen ->
                forAll gen $ \sets ->
                    forAll genValid $ \repo -> do
                        forAll genValid $ \note -> do
                            runData sets $ do
                                importRepo repo
                                void $ createNewNote note
                            assertRepoValid sets
    describe "createNewNoteUuid" $
        it "generates a NoteUuid that was not in the index yet, but is now" $
        forAll genValid $ \noteIndex -> do
            (newUuid, newIndex) <- createNewNoteUuid noteIndex
            newUuid `shouldNotSatisfy` containsNoteUuid noteIndex
            newUuid `shouldSatisfy` containsNoteUuid newIndex
    describe "subNoteIndex" $ do
        it "generates valid note indices" $
            forAll genValid $ genGeneratesValid . subNoteIndex
        it "generates sub note indices" $
            forAll genValid $ \ni ->
                forAll (subNoteIndex ni) $ \sni ->
                    sni `shouldSatisfy` (`isSubNoteIndexOf` ni)
