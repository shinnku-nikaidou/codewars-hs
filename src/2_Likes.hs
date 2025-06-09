module Likes where

import Test.Hspec

likes :: [String] -> String
likes [] = "no one likes this"
likes [n1] = n1 ++ " likes this"
likes [n1, n2] = n1 ++ " and " ++ n2 ++ " like this"
likes [n1, n2, n3] = n1 ++ ", " ++ n2 ++ " and " ++ n3 ++ " like this"
likes (n1 : n2 : _ : xs) = n1 ++ ", " ++ n2 ++ " and " ++ show (length xs + 1) ++ " others like this"

-- TODO

spec :: Spec
spec = do
  describe "Static tests" $ do
    it "Static test: empty" $ likes [] `shouldBe` "no one likes this"
    it "Static test: 1 name" $ likes ["Peter"] `shouldBe` "Peter likes this"
    it "Static test: 2 names" $ likes ["Jacob", "Alex"] `shouldBe` "Jacob and Alex like this"
    it "Static test: 3 names" $ likes ["Max", "John", "Mark"] `shouldBe` "Max, John and Mark like this"
    it "Static test: 4 names" $ likes ["Alex", "Jacob", "Mark", "Max"] `shouldBe` "Alex, Jacob and 2 others like this"
