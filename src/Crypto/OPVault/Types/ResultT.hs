{-# LANGUAGE LambdaCase #-}
module Crypto.OPVault.Types.ResultT where

import Control.Monad ((<=<), liftM)
import Control.Monad.Catch (MonadThrow(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Crypto.Error (CryptoFailable, eitherCryptoError)

data ResultT m a = ResultT { runResultT :: m (Either String a) }

instance Functor m => Functor (ResultT m) where
    fmap fn = ResultT . fmap (fmap fn) . runResultT

instance Monad m => Applicative (ResultT m) where
    pure           = ResultT . pure . Right
    resFn <*> resX = ResultT $ do
        fn <- runResultT resFn
        x  <- runResultT resX
        case (fn, x) of
          (Right fn',  Right x')  -> return . Right $ fn' x'
          (Left x, _) -> return $ Left x
          (_, Left x) -> return $ Left x

instance Monad m => Monad (ResultT m) where
    return    = pure
    mX >>= fn = ResultT $ do
        x  <- runResultT mX
        case x of
          Left err -> return $ Left err
          Right x' -> runResultT $ fn x'

instance MonadTrans ResultT where
    lift = ResultT . liftM Right

instance MonadIO m => MonadIO (ResultT m) where
    liftIO = ResultT . liftIO . fmap Right

instance Monad m => MonadThrow (ResultT m) where
    throwM e = failure (show e)

doResult :: ResultT IO () -> IO ()
doResult x =
    runResultT x >>=
    \case Left x   -> putStrLn x
          Right () -> return ()

failure :: Monad m => String -> ResultT m a
failure = ResultT . return . Left

catResults :: Monad m => [ResultT m a] -> m [a]
catResults (r:rs) =
    runResultT r >>=
    \case Left _  -> catResults rs
          Right x -> (x:) <$> catResults rs
catResults [] = return []

liftEither :: (Show s, Monad m) => Either s a -> ResultT m a
liftEither (Left l)  = ResultT . return . Left $ show l
liftEither (Right r) = ResultT . return $ Right r

liftEitherM :: (Show s, Monad m) => m (Either s a) -> ResultT m a
liftEitherM = liftEither <=< lift

liftMaybe :: Monad m => String -> Maybe a -> ResultT m a
liftMaybe _   (Just x) = ResultT . return $ Right x
liftMaybe str _        = ResultT . return $ Left  str

liftMaybeT :: Monad m => String -> m (Maybe a) -> ResultT m a
liftMaybeT str = liftMaybe str <=< lift

liftCrypto :: Monad m => CryptoFailable a -> ResultT m a
liftCrypto c =
    case eitherCryptoError c of
      Left  l -> failure $ show l
      Right r -> return r
