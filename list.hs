import Data.Char

myWords :: String -> [String]
myWords s 
    |  s == [] = [] 
    | otherwise = takeWhile (/=' ') s : (myWords.drop 1 . dropWhile (/=' ') $ s)

filterUpper :: String -> String
filterUpper a = filter isUpper a