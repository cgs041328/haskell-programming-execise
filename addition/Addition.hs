module Addition where

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "2 * 0 is 0" $ do
            multiply' 2 0 `shouldBe` 0
        it "2 * 3 = 6 " $ do
            multiply' 2 3 `shouldBe` 6

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