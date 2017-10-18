import Data.Time
data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
            [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
            , DbNumber 9001
            , DbString "Hello, world!"
            , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
            ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr (\ a b -> case a of DbDate t -> t : b
                                         _          -> b ) []

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

stops = "pbtdkg"
vowels = "aeiou"

makeWords = [ x : y : z : [] | x <- stops , y <-vowels , z <- stops]


seekritFunc x =
    div (sum (map length (words x)))
    (length (words x))

