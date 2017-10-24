module VigenereCipher where
    
import Data.Char
      

zipWith' :: (Char -> Char -> Char) -> String -> String -> String
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys)
    | x == ' ' = x : zipWith' f xs (y:ys)
    | otherwise = f x y : zipWith' f xs ys


shift :: Char -> Char -> Char
shift ' ' _ = ' '
shift a b = chr $ ((ord a - ordA + ord b - ordA) `mod` 26) + ordA
    where ordA = ord 'A'


encode :: String -> String
encode s = zipWith'  shift s keywordStream
    where keywordStream = concat $ repeat keyword
          keyword = "ALLY"

testEncode =
    if encode "MEET AT DAWN" == "MPPR AE OYWY"
    then print "yup okay!"
    else error "test failed!"