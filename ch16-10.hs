import Test.QuickCheck
import Test.QuickCheck.Function

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a c d b) = Four' a c d (f b)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main = do
  quickCheck $ \x -> functorIdentity (x :: [Int])
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck (functorCompose :: IntFC)