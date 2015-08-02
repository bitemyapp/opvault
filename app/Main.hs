{-# LANGUAGE OverloadedStrings, RecordWildCards, LambdaCase #-}
module Main where

import Control.Concurrent
import Control.Monad (when, void)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B (getLine)
import qualified Data.Text.IO as T (putStr, putStrLn, getLine)
import System.Environment (getEnv)
import System.IO (hSetEcho, stdout, hFlush)

import Crypto.OPVault

type CtxT m a = ReaderT Ctx' (ResultT m) a

type CtxRun m a = CtxT m a -> ResultT m a

data Ctx' = Ctx'
    { vault     :: Vault
    , profile   :: Profile
    , folderMap :: FolderMap
    , itemMap   :: ItemMap
    , itemIndex :: ItemIndex
    , store     :: PasswordStore
    }

newtype PasswordStore = PasswordStore (MVar (ThreadId, Password))

newStore :: MonadIO m => m PasswordStore
newStore = io $ PasswordStore <$> newEmptyMVar

storePassword :: MonadIO m => PasswordStore -> Password -> m ()
storePassword (PasswordStore mv) pw = io $ do
    tryTakeMVar mv >>=
        \case Just (tid, _) -> killThread tid
              Nothing       -> return ()
    tid <- forkIO . void $ threadDelay (5 * 60 * 1000000) >> tryTakeMVar mv
    putMVar mv (tid, pw)

password :: MonadIO m => PasswordStore -> m Password
password (PasswordStore mv) = io $ do
    stored <- tryTakeMVar mv
    pw     <- case stored of
      Nothing       -> promptPassword
      Just (tid, p) -> killThread tid >> return p
    tid <- forkIO . void $ threadDelay (5 * 60 * 1000000) >> tryTakeMVar mv
    putMVar mv (tid, pw)
    return pw

flushAnd :: MonadIO m => IO a -> m a
flushAnd x = io (hFlush stdout >> x)

promptPath :: MonadIO m => ResultT m (Vault, Profile)
promptPath = do
    io $ putStr "Enter 1Password Vault Path "
    io $ putStr "[~/Dropbox/1Password/1Password.opvault]: "
    path <- flushAnd getLine >>=
        \case ""       -> (++ "/Dropbox/1Password/1Password.opvault") <$> io (getEnv "HOME")
              ('~':x)  -> (++ x) <$> io (getEnv "HOME")
              x        -> return x
    getVault $ VaultPath (path ++ "/default")

promptPassword :: MonadIO m => m Password
promptPassword = io $ do
    putStr "Enter vault password: "
    hSetEcho stdout False
    pass <- flushAnd B.getLine
    hSetEcho stdout True
    putStrLn []
    return $ Password pass

promptIndexKey :: MonadIO m => ItemIndex -> ResultT m Item
promptIndexKey idx = do
    key <- io $ do
        putStr "Enter a vault item name: "
        flushAnd T.getLine
    when (key == "") $ failure ""
    liftMaybe "Could not find requested item" $
        itemLookup (Title key) idx

runCtx :: MonadIO m => ResultT m (CtxRun m ())
runCtx = do
    store            <- newStore
    (vault, profile) <- promptPath
    folderMap        <- getFolderFile vault
    itemMap          <- getItems vault
    itemIndex        <- makeItemIndex itemMap =<<
                        overviewKey profile . derivedKey profile =<<
                        password store
    return $ flip runReaderT Ctx'{..}

showCredentials :: MonadIO m => ItemDetails -> m ()
showCredentials ItemDetails{..} = do
    let names  = filter ((`elem` [UsernameField, EmailField]) . iType) iFields
    let passes = filter ((==PasswordField) . iType) iFields
    io $ case (names, passes) of
      ([], _)    -> putStrLn "No login credentials for item."
      (_, [])    -> putStrLn "No login credentials for item."
      (x:_, y:_) -> do
          T.putStr "\nUsername: "
          T.putStrLn $ iValue x
          T.putStr "Password: "
          T.putStrLn $ iValue y
          T.putStr "\n"

lookupItem :: MonadIO m => CtxT m ()
lookupItem = do
    Ctx'{..} <- ask
    item     <- lift $ promptIndexKey itemIndex
    itemKey  <- lift $
                itemKey item =<<
                masterKey profile . derivedKey profile =<<
                password store
    details  <- lift $ itemDetails item itemKey
    showCredentials details

loop :: MonadIO m => CtxRun m () -> CtxT m () -> ResultT m ()
loop run action = do
    r <- lift $ runResultT (run action)
    case r of
      Left ""  -> io $ putStrLn "Exiting..."
      Left err -> io (putStrLn $ "Error: " ++ err) >>
                  loop run action
      Right () -> loop run action

main :: IO ()
main = doResult $ do
    runner <- runCtx :: ResultT IO (CtxT IO () -> ResultT IO ())
    loop runner lookupItem
