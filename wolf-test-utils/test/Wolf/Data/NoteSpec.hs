module Wolf.Data.NoteSpec
    ( spec
    ) where

import TestImport

import Wolf.Data.Note

import Wolf.Data.TestUtils

import Wolf.Data.Note.Types.Gen ()
import Wolf.Data.Types.Gen ()

spec :: Spec
spec =
    withDataSetsGen $ do
        describe "readPersonNote" $ do
            it "gets 'Nothing' if the given note does not exist" $ \gen ->
                forAll gen $ \sets ->
                    forAll genUnchecked $ \personNoteUuid -> do
                        mpn <- runReaderT (readNote personNoteUuid) sets
                        mpn `shouldBe` Nothing
            it "reads the note that was just written" $ \gen ->
                forAll gen $ \sets ->
                    forAll genUnchecked $ \personNoteUuid ->
                        forAll genValid $ \personNote -> do
                            pn <-
                                flip runReaderT sets $ do
                                    writeNote personNoteUuid personNote
                                    readNote personNoteUuid
                            pn `shouldBe` Just personNote
        describe "writePersonNote" $
            it "does not crash" $ \gen ->
                forAll gen $ \sets ->
                    forAll genUnchecked $ \personNoteUuid ->
                        forAll genUnchecked $ \personNote ->
                            runReaderT
                                (writeNote personNoteUuid personNote)
                                sets `shouldReturn`
                            ()
