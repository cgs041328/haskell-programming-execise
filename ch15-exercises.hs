import Data.Monoid(Monoid)
import Data.Semigroup(Semigroup, (<>))
import Test.QuickCheck

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

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
       

