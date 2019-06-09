{-# LANGUAGE TypeApplications #-}

module Wolf.Data.SuggestionSpec
  ( spec
  ) where

import TestImport

import qualified Data.Map as M
-- import qualified Data.Set as S

import Wolf.Data.Suggestion
import Wolf.Data.TestUtils

import Wolf.Data.Gen ()

spec :: Spec
spec = do
  describe "hashSuggestion" $
    it "hashes the same entry to the same hash" $ producesValidsOnValids (hashSuggestion @Int)
  withDataSetsGen $ do
    describe "readUnusedSuggestions" $ do
      -- it "reads the suggestions that were just written" $
      --   forAllSets $ \sets ->
      --     forAllValid $ \typ ->
      --       forAllValid $ \sugs -> do
      --         sugs' <-
      --           runData sets $ do
      --             ensureClearRepository
      --             writeUnusedSuggestions @Int typ sugs
      --             readUnusedSuggestions @Int typ
      --         M.map suggestionData sugs' `shouldBe` M.map suggestionData sugs'
      -- it "reads the suggestions that were just added" $
      --   forAllSets $ \sets ->
      --     forAllValid $ \typ ->
      --       forAllValid $ \sugs -> do
      --         sugs' <-
      --           runData sets $ do
      --             ensureClearRepository
      --             addUnusedSuggestions @Int typ sugs
      --             readUnusedSuggestions @Int typ
      --         S.fromList (M.elems sugs') `shouldBe` sugs
      it "contains the suggestion that was just added" $
        forAllSets $ \sets ->
          forAllValid $ \typ ->
            forAllValid $ \sug -> do
              sugs' <-
                runData sets $
                                    -- Don't clear first
                 do
                  addUnusedSuggestion @Int typ sug
                  readUnusedSuggestions @Int typ
              suggestionData sug `shouldSatisfy` (`elem` M.elems (M.map suggestionData sugs'))
    describe "readUsedSuggestions" $
      -- it "reads the suggestions that were just written" $
      --   forAllSets $ \sets ->
      --     forAllValid $ \typ ->
      --       forAllValid $ \sugs -> do
      --         sugs' <-
      --           runData sets $ do
      --             ensureClearRepository
      --             writeUsedSuggestions @Int typ sugs
      --             readUsedSuggestions @Int typ
      --         S.fromList (M.elems sugs') `shouldBe` sugs
      -- it "reads the suggestions that were just added" $
      --   forAllSets $ \sets ->
      --     forAllValid $ \typ ->
      --       forAllValid $ \sugs -> do
      --         sugs' <-
      --           runData sets $ do
      --             ensureClearRepository
      --             recordUsedSuggestions @Int typ sugs
      --             readUsedSuggestions @Int typ
      --         S.fromList (M.elems sugs') `shouldBe` sugs
     do
      it "contains the suggestion that was just added" $
        forAllSets $ \sets ->
          forAllValid $ \typ ->
            forAllValid $ \sug -> do
              sugs' <-
                runData sets $
                                    -- Don't clear first
                 do
                  recordUsedSuggestion @Int typ sug
                  readUsedSuggestions @Int typ
              suggestionData sug `shouldSatisfy` (`elem` M.elems (M.map suggestionData sugs'))
    describe "recordUsed" $
      it "always adds the suggestion to the used map if the maps were empty" $
      forAllSets $ \sets ->
        forAllValid $ \typ ->
          forAllValid $ \sug -> do
            (uusi, usi) <- runData sets $ recordUsed @Int typ (M.empty, M.empty) sug
            uusi `shouldBe` M.empty
            hashSuggestion sug `shouldSatisfy` (`M.member` usi)
            M.size usi `shouldBe` 1
        -- describe "readAllSuggestions" $
        --     it "reads the suggestions that were just written" $
        --     forAllSets $ \sets ->
        --         forAllValid $ \sugs -> do
        --             sugs' <-
        --                 runData sets $ do
        --                     ensureClearRepository
        --                     writeAllSuggestions sugs
        --                     readAllSuggestions
        --             sugs' `shouldBe` sugs
