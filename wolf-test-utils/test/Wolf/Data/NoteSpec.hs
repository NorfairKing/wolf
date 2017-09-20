module Wolf.Data.NoteSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Note

import Wolf.Data.TestUtils

import Wolf.Data.Gen ()

spec :: Spec
spec =
    withDataSetsGen $ do
        describe "readPersonNote" $ do
            it "gets 'Nothing' if the given note does not exist" $ \gen ->
                forAll gen $ \sets ->
                    forAllUnchecked $ \personNoteUuid -> do
                        mpn <- runReaderT (readNote personNoteUuid) sets
                        mpn `shouldBe` Nothing
            it "reads the note that was just written" $ \gen ->
                forAll gen $ \sets ->
                    forAllUnchecked $ \personNoteUuid ->
                        forAllValid $ \personNote -> do
                            pn <-
                                flip runReaderT sets $ do
                                    writeNote personNoteUuid personNote
                                    readNote personNoteUuid
                            pn `shouldBe` Just personNote
        describe "writePersonNote" $
            it "does not crash" $ \gen ->
                forAll gen $ \sets ->
                    forAllUnchecked $ \personNoteUuid ->
                        forAllUnchecked $ \personNote ->
                            runReaderT
                                (writeNote personNoteUuid personNote)
                                sets `shouldReturn`
                            ()
