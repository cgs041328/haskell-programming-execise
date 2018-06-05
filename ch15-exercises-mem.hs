import Data.Monoid (Monoid, (<>))

--Mem
newtype Mem s a =
    Mem {
    runMem :: s -> (a,s)
    }

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \s -> (mempty, s)
    mappend Mem {runMem = f} Mem {runMem = g} = Mem $ \x -> let (s1, a1) = g x
                                                                (s2, a2) = f a1
                                                            in (s1 <> s2, a2)

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0