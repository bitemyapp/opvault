module Crypto.OPVault.Common.ResultT where

import Control.Applicative (Applicative(..))
import Control.Monad ((<=<), liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Crypto.Error (CryptoFailable, eitherCryptoError)

data ResultT m a = ResultT { runResultT :: m (Either String a) }

instance Functor m => Functor (ResultT m) where
    fmap fn = ResultT . fmap (fmap fn) . runResultT

instance (Applicative m, Monad m) => Applicative (ResultT m) where
    pure           = ResultT . pure . Right
    resFn <*> resX = ResultT $ do
        fn <- runResultT resFn
        x  <- runResultT resX
        case (fn, x) of
          (Left errFn, Left errX) -> return . Left  $ concat [errFn, "\n", errX]
          (Right fn',  Right x')  -> return . Right $ fn' x'
          (Left errFn, Right _)   -> return $ Left errFn
          (Right _,    Left errX) -> return $ Left errX

instance (Applicative m, Monad m) => Monad (ResultT m) where
    return    = pure
    mX >>= fn = ResultT $ do
        x  <- runResultT mX
        case x of
          Left err -> return $ Left err
          Right x' -> runResultT $ fn x'

instance MonadTrans ResultT where
    lift = ResultT . liftM Right

instance (Applicative m, MonadIO m) => MonadIO (ResultT m) where
    liftIO = ResultT . liftIO . fmap Right

failure :: Monad m => String -> ResultT m a
failure = ResultT . return . Left

liftEither :: (Show s, Monad m) => Either s a -> ResultT m a
liftEither (Left l)  = ResultT . return . Left $ show l
liftEither (Right r) = ResultT . return $ Right r

liftEitherM :: (Show s, Applicative m, Monad m) => m (Either s a) -> ResultT m a
liftEitherM = liftEither <=< lift

liftMaybe :: Monad m => String -> Maybe a -> ResultT m a
liftMaybe _   (Just x) = ResultT . return $ Right x
liftMaybe str _        = ResultT . return $ Left  str

liftMaybeT :: (Applicative m, Monad m) => String -> m (Maybe a) -> ResultT m a
liftMaybeT str = liftMaybe str <=< lift

liftCrypto :: (Applicative m, Monad m) => CryptoFailable a -> ResultT m a
liftCrypto c =
    case eitherCryptoError c of
      Left  l -> failure $ show l
      Right r -> return r
