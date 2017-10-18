module Cipher where
    import Data.Char

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x:xs) = myReverse xs ++ [x]

    squish :: [[a]] -> [a]
    squish [] = []
    squish (x:xs) = x ++ squish xs
