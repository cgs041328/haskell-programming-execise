import Criterion.Main

newtype DList a = DL { unDL :: [a] -> [a] }

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL (a:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList xs = unDL xs []
{-# INLINE toList #-}

-- Prepend a single element to a dlist.
infixr `cons`
cons :: a -> DList a -> DList a
cons x xs =  singleton x `append` xs 
{-# INLINE cons #-}

-- Append a single element to a dlist.
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = xs `append` singleton x 
{-# INLINE snoc #-}

-- Append dlists.
append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
    where go 0 xs = xs
          go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
    where go 0 xs = xs
          go n xs =
            go (n-1)
            (singleton n `append` xs)

main :: IO ()
main = defaultMain
    [ bench "concat list" $
        whnf schlemiel 123456
    , bench "concat dlist" $
        whnf constructDlist 123456
    ]

-- From Okasaki's Purely
-- Functional Data Structures
data Queue a =
    Queue { enqueue :: [a]
          , dequeue :: [a]
          } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x q = q { enqueue = x : enqueue q}

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue [] []) = Nothing
pop q = case dequeue q of
    []     -> let (x : xs) = reverse . enqueue $ q in Just (x, Queue [] xs)
    (x:xs) -> Just (x, q { dequeue = xs})