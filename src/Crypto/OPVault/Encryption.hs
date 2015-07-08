{-# LANGUAGE RecordWildCards #-}
module Crypto.OPVault.Encryption where


import Prelude hiding (drop, length, take)
import Data.ByteString (drop, length, take)
import Data.String (IsString(..))
import Data.Text.Encoding (encodeUtf8)

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cbcDecrypt, cipherInit, makeIV)
import Crypto.Hash.Algorithms (SHA512(..))
import Crypto.KDF.PBKDF2 (Parameters(..), generate, prfHMAC)

import Crypto.OPVault.Common
import Crypto.OPVault.Types

newtype Password = Password ByteString

instance Show Password where
    show = const "Password <...>"

instance IsString Password where
    fromString = Password . encodeUtf8 . fromString

data DerivedKey = DerivedKey
    { dKey :: ByteString
    , dMAC :: ByteString
    } deriving Eq

instance Show DerivedKey where
    show = const "DerivedKey <...>"

data MasterKey = MasterKey
    { mKey :: ByteString
    , mMAC :: ByteString
    }

instance Show MasterKey where
    show = const "MasterKey <...>"

derivedKey :: Profile -> Password -> DerivedKey
derivedKey Profile{..} (Password pass) =
    let bytes = generate (prfHMAC SHA512)
                         (Parameters pIterations 512)
                         pass
                         (rawBytes pSalt)
     in DerivedKey (take 32 bytes) (drop 32 bytes)

masterKey :: (Applicative m, Monad m) => Opdata01 -> DerivedKey -> ResultT m MasterKey
masterKey Opdata01{..} DerivedKey{..} = do
    ctx <- liftCrypto $ cipherInit dKey
    iv  <- liftMaybe "Could not create IV" $ makeIV oIV
    let bytes = cbcDecrypt (ctx::AES256) iv oData
    if length bytes /= 256
       then failure "Badly sized master key"
       else return $ MasterKey (take 32 bytes) (take 32 $ drop 32 bytes)
{-
overviewKey :: Monad m => Password -> Profile -> ResultT m (Key Overview)
overviewKey = undefined
-}
