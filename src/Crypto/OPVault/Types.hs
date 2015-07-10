{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
module Crypto.OPVault.Types
   ( module Crypto.OPVault.Types
   , module Common
   ) where

import Crypto.OPVault.Types.Base64     as Common
import Crypto.OPVault.Types.Common     as Common
import Crypto.OPVault.Types.Encryption as Common
import Crypto.OPVault.Types.FileTypes  as Common
import Crypto.OPVault.Types.ItemIndex  as Common
import Crypto.OPVault.Types.Opdata01   as Common
import Crypto.OPVault.Types.ResultT    as Common

import Control.Concurrent.Async (Concurrently(..))

newtype Vault = VaultPath String deriving (Show, Eq)

instance IsString Vault where
    fromString = VaultPath

newtype ConcurrentlyT (m :: * -> *) a = ConcurrentlyT (Concurrently a)
    deriving (Functor, Applicative, Monad)

runConcurrentlyT :: MonadIO m => ConcurrentlyT m a -> m a
runConcurrentlyT (ConcurrentlyT c) = io $ runConcurrently c

io :: MonadIO m => IO a -> m a
io = liftIO

io' :: MonadIO m => IO a -> ConcurrentlyT m a
io' = ConcurrentlyT . Concurrently
