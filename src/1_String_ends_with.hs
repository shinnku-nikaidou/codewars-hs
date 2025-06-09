module StringsEndsWith (solution) where
import Data.List
import Test.Hspec

solution :: String -> String -> Bool
solution _ [] = True
solution [] _ = False
solution x y
  | last x == last y = solution (delete_last x) (delete_last y)
  | otherwise = False
  where 
    delete_last xs = take (length xs - 1) xs

--

spec :: Spec
spec = do
  it "example tests" $ do
    solution "abcde" "cde" `shouldBe` True
    solution "abcde" "abc" `shouldBe` False
    solution "abcde" "" `shouldBe` True
