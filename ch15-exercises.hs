import Data.Monoid(Monoid)
import Data.Semigroup(Semigroup, (<>), Sum(Sum, getSum))
import Test.QuickCheck(Arbitrary, arbitrary, quickCheck, elements)

--Trivial
data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

--Identity
newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup ( Identity a ) where
    Identity a <> Identity b = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do 
        a <- arbitrary
        return (Identity a)

--Two
data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup(Two a b) where
    (Two a b) <> (Two x y) = Two (a <> x) (b <> y)
    
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do 
        a <- arbitrary
        b <- arbitrary
        return (Two a b) 

--BoolConj
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True
    _   <>  _  = BoolConj False

instance Arbitrary BoolConj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolConj a), (BoolConj a)]

--BoolDisj
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj a) <> (BoolDisj b) = BoolDisj (a || b)

instance Arbitrary BoolDisj where
    arbitrary = do
        a <- arbitrary
        elements [(BoolDisj a), (BoolDisj a)]

--Or
data Or a b =
    Fst a
    | Snd b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Fst _) <> b       = b
    (Snd _) <> (Snd b) = Snd b
    a <> _             = a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [(Fst a), (Snd b)]

--Combine
newtype Combine a b =
    Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
    Combine {unCombine=f} <> Combine {unCombine=g} = Combine (f <> g)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

--Validation
data Validation a b =
    Failure a | Success b
    deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
   Success a <> _         = Success a 
   _         <> Success b = Success b
   Failure a <> Failure b = Failure (a <> b)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

type TwoAssoc = Two String Ordering -> Two String Ordering -> Two String Ordering -> Bool

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

type OrAssoc = Or String Ordering -> Or String Ordering -> Or String Ordering -> Bool


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  print $ unCombine (f <> g ) $ 0
  print $ unCombine (f <> g ) $ 1
  print $ unCombine (f <> f ) $ 1
  print $ unCombine (g <> f ) $ 1
  let failure :: String -> Validation String Int
      failure = Failure
      success :: Int -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
  
  
  
       

