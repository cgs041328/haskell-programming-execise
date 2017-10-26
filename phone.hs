import Data.Char
import Data.List
import Data.Tuple (swap)
import Data.Function (on)

type PhoneNumber = Char
type PhoneCharacter = String
data DaPhone = DaPhone [(PhoneNumber,PhoneCharacter)] deriving (Eq, Show)

daphone :: DaPhone
daphone = DaPhone 
        [('1', "1"),
        ('2', "abc"),
        ('3', "def"),
        ('4', "ghi"),
        ('5', "jkl"),
        ('6', "mno"),
        ('7', "pqrs"),
        ('8', "tuv"),
        ('9', "wxyz"),
        ('*', "*^"),
        ('0', " +_"),
        ('#', "#.,")]

convo :: [String]
convo =
    ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]


indexOf :: Eq a =>  a -> [a] -> Int
indexOf x xs = case index of
  Just v  -> v + 1
  Nothing -> 0
  where index = elemIndex x xs 
-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone x) v = foldr f [] y
    where y = map swap x
          g = (\(c,d) acc -> if isUpper v && elem (toLower v) c then ('*', 1) : (d, indexOf (toLower v) c) : acc else acc)
          f = (\(c,d) acc -> if elem v c then (d, indexOf v c) : acc else g (c,d) acc)
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead d = concat.map (reverseTaps d)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_,p) b -> p + b) 0

-- from SO
frequency :: Ord a => [a] -> [(Int,a)] 
frequency list = map (\l -> (length l, head l)) (group (sort list))

mostPopularLetter ::Ord a => [a] -> a
mostPopularLetter s = snd $ maximumBy (compare `on` fst)(frequency s)

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter.concat

coolestWord :: [String] -> String
coolestWord  = mostPopularLetter