import Data.List
-- >>> notThe "the"
-- Nothing
-- >>> notThe "blahtheblah"
-- Just "blahtheblah"
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe s
    | s == "the" = Nothing
    | otherwise  = Just s
-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe str = concat $ intersperse " " $ map f $ fmap notThe $ words str
    where f Nothing   = "a"
          f (Just s)  = s  


-- >>> countTheBeforeVowel "the cow"
-- 0
-- >>> countTheBeforeVowel "the evil cow"
-- 1
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = f 0.words
    where f n (x:y:ys)
            | x == "the" && isVowel (head y) = f (n + 1) (y:ys)
            | otherwise = f n ys
          f n _ = n

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

countVowels :: String -> Int
countVowels = length.filter isVowel

newtype Word' =
    Word' String
    deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord w = if lv < lc then Just (Word' w) else Nothing
    where lv = length $ filter ((flip elem) vowels) w
          lc = length w - lv

-- As natural as any competitive bodybuilder
data Nat =
    Zero
    | Succ Nat
    deriving (Eq, Show)
-- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n
-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat i
    | i<0 = Nothing
    | i == 0 = Just Zero
    | otherwise = Just (Succ succ)
        where Just succ = integerToNat (i-1)

-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False
-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not.isJust

-- >>> mayybee 0 (+1) Nothing
-- 0
-- >>> mayybee 0 (+1) (Just 1)
-- 2
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee b _ _ = b

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just a) = a
fromMaybe a _ = a

-- >>> listToMaybe [1, 2, 3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x
-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = [] 
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Just x:xs) = x:catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs
-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe xs = foldr f (Just []) xs
    where f _ Nothing = Nothing
          f Nothing _ = Nothing
          f (Just a) (Just b) = Just (a:b)

lefts' :: [Either a b] -> [a]
lefts' = foldr f [] 
    where f (Left a) xs = a:xs
          f _ xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
    where f (Left a) xs = xs
          f (Right b) xs = b : xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([],[])
    where f (Left a) (as, bs) = (a:as, bs)
          f (Right b) (as, bs) = (as, b:bs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (Just .f) e

myIterate :: (a -> a) -> a -> [a]
myIterate f a =  a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
                Nothing -> []
                Just(x, y) -> x : myUnfoldr f y

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

data BinaryTree a =
    Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
                Nothing -> Leaf
                Just(x,y,z) -> Node (unfold f x) y (unfold f z)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
    where f a
            | a == n = Nothing
            | otherwise = Just(a + 1, a,a + 1)