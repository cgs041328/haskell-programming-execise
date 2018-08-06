import Control.Monad

bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma = join $ fmap f ma

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f ma = ma >>= (\a -> return (f a))

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = (ma >>= (\a -> return $ f a)) <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = (++) <$> (fmap (\x->[x]) $ f x) <*> meh xs f

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id