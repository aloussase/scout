module Main where

import           Data.Either (isLeft, isRight)

import           Scout

import           Test.Hspec


-- TODO: Test that the resulting Options have the right shape

searchOptionsSpec :: Spec
searchOptionsSpec = do
    context "Search options" $ do
        describe "No options provided" $ do
            it "Can parse when given search query" $
                toOptions ["search", "megaparsec"] `shouldSatisfy`  isRight
            it "Fails when not provided a query string" $
                toOptions ["search"] `shouldSatisfy` isLeft

        describe "Sort direction options" $ do
            it "Can parse valid sort directions" $ do
                toOptions ["search", "", "--sort", "ascending"] `shouldSatisfy` isRight
                toOptions ["search", "", "--sort", "descending"] `shouldSatisfy` isRight
            it "Fails on invalid sort options" $
                toOptions ["search", "", "--sort", "not-a-valid-direction"] `shouldSatisfy` isLeft
            it "Fails when not given an argument" $
                toOptions ["search", "", "--sort"] `shouldSatisfy` isLeft

formatOptionsSpec :: Spec
formatOptionsSpec = do
    context "Format options" $ do
        describe "Limit option" $ do
            it "Can parse a valid limit number" $
                toOptions ["search", "", "--limit", "5"] `shouldSatisfy` isRight
            it "Fails to parse an invalid limit number" $
                toOptions ["search", "", "--limit", "nan"] `shouldSatisfy` isLeft

        describe "Output format options" $ do
            it "Can parse valid formats" $ do
                toOptions ["search", "", "--format", "apt"] `shouldSatisfy` isRight
                toOptions ["search", "", "--format", "csv"] `shouldSatisfy` isRight
            it "Fails to parse invalid formats" $
                toOptions ["search", "", "--format", "yaml"] `shouldSatisfy` isLeft

        describe "Field selection option" $ do
            it "Can parse all fields" $
                toOptions ["search", "", "--select", "description,downloads,lastUpload,name,uri,votes"] `shouldSatisfy` isRight
            it "Fails to parse invalid fields" $
                toOptions ["search", "", "--select", "stars"] `shouldSatisfy` isLeft

optionsSpec :: Spec
optionsSpec = searchOptionsSpec >> formatOptionsSpec

main :: IO ()
main = hspec optionsSpec
