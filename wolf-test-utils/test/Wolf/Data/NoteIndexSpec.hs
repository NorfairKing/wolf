module Wolf.Data.NoteIndexSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Import
import Wolf.Data.Note
import Wolf.Data.NoteIndex

import Wolf.Data.Gen
import Wolf.Data.TestUtils

spec :: Spec
spec = do
    withDataSetsGen $ do
        describe "getNoteIndex" $
            it "retrieves the note index that was just written" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \noteIndex -> do
                        ni <-
                            flip runReaderT sets $ do
                                putNoteIndex noteIndex
                                getNoteIndex
                        ni `shouldBe` noteIndex
        describe "putNoteIndex" $
            it "does not crash" $ \gen ->
                forAll gen $ \sets ->
                    forAllUnchecked $ \noteIndex ->
                        runReaderT (putNoteIndex noteIndex) sets `shouldReturn`
                        ()
        describe "getPersonNoteIndex" $
            it "retrieves the note index that was just written" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \personUuid ->
                        forAllValid $ \noteIndex -> do
                            ni <-
                                flip runReaderT sets $ do
                                    putPersonNoteIndex personUuid noteIndex
                                    getPersonNoteIndex personUuid
                            ni `shouldBe` Just noteIndex
        describe "putPersonNoteIndex" $
            it "does not crash" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \personUuid ->
                        forAllUnchecked $ \noteIndex ->
                            runReaderT
                                (putPersonNoteIndex personUuid noteIndex)
                                sets `shouldReturn`
                            ()
        describe "createNewNoteUuid" $ do
            it "does not crash and generates a valid noteUuid" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \note -> do
                        noteUuid <- runReaderT (createNewNote note) sets
                        noteUuid `shouldSatisfy` isValid
            it "will leave the new note uuid in the global note index" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \note -> do
                        (noteUuid, gni) <-
                            flip runReaderT sets $
                            (,) <$> createNewNote note <*> getNoteIndex
                        gni `shouldSatisfy` (`containsNoteUuid` noteUuid)
            it
                "will leave the new note uuid in the each of the relevant people's note index" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \note -> do
                        (noteUuid, pnis) <-
                            flip runReaderT sets $ do
                                nid <- createNewNote note
                                pnis <-
                                    forM (toList $ noteRelevantPeople note) $ \personUuid ->
                                        (,) personUuid <$>
                                        getPersonNoteIndexWithDefault personUuid
                                pure (nid, pnis)
                        forM_ pnis $ \(_, personNoteIndex) ->
                            personNoteIndex `shouldSatisfy`
                            (`containsNoteUuid` noteUuid)
        describe "createNewNote" $ do
            it "adds exactly one new note to the global note index: the new one" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \repo ->
                        forAllValid $ \note -> do
                            (old, n, new) <-
                                runData sets $ do
                                    importRepo repo
                                    old <- getNoteIndex
                                    n <- createNewNote note
                                    new <- getNoteIndex
                                    pure (old, n, new)
                            addToNoteIndex old n `shouldBe` Just new
            it
                "adds exactly one new note uuid to each person's note index: the new one" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \repo ->
                        forAllValid $ \note -> do
                            (olds, n, news) <-
                                runData sets $ do
                                    importRepo repo
                                    olds <-
                                        mapM
                                            getPersonNoteIndexWithDefault
                                            (toList $ noteRelevantPeople note)
                                    n <- createNewNote note
                                    news <-
                                        mapM
                                            getPersonNoteIndexWithDefault
                                            (toList $ noteRelevantPeople note)
                                    pure (olds, n, news)
                            forM_ (zip olds news) $ \(old, new) ->
                                addToNoteIndex old n `shouldBe` Just new
            it "leaves a valid repository valid" $ \gen ->
                forAll gen $ \sets ->
                    forAllValid $ \repo ->
                        forAllValid $ \note -> do
                            runData sets $ do
                                importRepo repo
                                void $ createNewNote note
                            assertRepoValid sets
    describe "createNewNoteUuid" $
        it "generates a NoteUuid that was not in the index yet, but is now" $
        forAllValid $ \noteIndex -> do
            (newUuid, newIndex) <- createNewNoteUuid noteIndex
            newUuid `shouldNotSatisfy` containsNoteUuid noteIndex
            newUuid `shouldSatisfy` containsNoteUuid newIndex
    describe "subNoteIndex" $ do
        it "generates valid note indices" $
            forAllValid $ \ni -> genGeneratesValid (subNoteIndex ni) shrinkValid
        it "generates sub note indices" $
            forAllValid $ \ni ->
                forAll (subNoteIndex ni) $ \sni ->
                    sni `shouldSatisfy` (`isSubNoteIndexOf` ni)
