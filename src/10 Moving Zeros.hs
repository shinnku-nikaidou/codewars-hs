module MovingZeros (moveZeros) where

import Test.Hspec

moveZeros :: [Int] -> [Int]
moveZeros l = uncurry (++) $ foldr step ([], []) l

step :: Int -> ([Int], [Int]) -> ([Int], [Int])
step x (a, b)
  | x == 0 = (a, 0 : b)
  | otherwise = (x : a, b)

spec :: Spec
spec = do
  it "example tests" $ do
    moveZeros [1, 2, 0, 1, 0, 1, 0, 3, 0, 1] `shouldBe` [1, 2, 1, 1, 3, 1, 0, 0, 0, 0]