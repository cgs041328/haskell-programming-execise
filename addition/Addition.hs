module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "2 * 0 is 0" $ do
            multiply' 2 0 `shouldBe` 0
        it "2 * 3 = 6 " $ do
            multiply' 2 3 `shouldBe` 6
        it "x + 1 is always\
            \ greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

sayHello :: IO ()
sayHello = putStrLn "hello!"

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise =
                go (n - d) d (count + 1)

multiply' :: (Eq a, Num a) => a -> a -> a
multiply' _ 0  = 0
multiply' a b  = a + multiply' a (b - 1)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater