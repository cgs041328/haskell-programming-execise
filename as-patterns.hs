import Data.Char
import Data.List


isSubsequenceOf' :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' xs@(x:xs') ys@(y:ys')  
                        | x == y     = isSubsequenceOf' xs' ys
                        | otherwise  = isSubsequenceOf' xs ys' && isSubsequenceOf' xs' ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x,capitalizeWord x)) . words 

capitalizeWord :: String -> String
capitalizeWord []    = []
capitalizeWord (x: xs) 
    | x == ' '  = x : capitalizeWord xs
    | otherwise = toUpper x : xs


splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs  = (takeWhile (/=c) xs) : splitOn c (drop 1 $ dropWhile (/=c) xs)

capitalizeParagraph :: String -> String
capitalizeParagraph = concat.intersperse "." .map capitalizeWord.splitOn '.'


