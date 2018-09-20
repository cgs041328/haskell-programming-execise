import  Control.Monad.Trans.Reader
import  Data.Functor.Identity
import  Control.Monad.Trans.State


rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a)
                => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do 
                        print $ "Hi: " ++ show a
                        return $ a + 1    

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \a -> do
                        print $ "Hi: " ++ show a
                        return $ (show a, a + 1)    