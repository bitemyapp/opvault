{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault.Encryption where

import Prelude hiding (drop, length, take)
import Data.ByteString (drop, take)
import Data.ByteArray (ByteArrayAccess, convert, length, View, view)
import Data.String (IsString(..))
import Data.Text.Encoding (encodeUtf8)

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cbcDecrypt, cipherInit, makeIV)
import Crypto.Hash (SHA512(..), Digest, hash)
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

type SHA512' = View (Digest SHA512)

data MasterKey = MasterKey
    { mKey :: ByteString
    , mMAC :: ByteString
    , mIV  :: ByteString
    }

instance Show MasterKey where
    show = const "MasterKey <...>"

data ItemKey = ItemKey
    { iKey :: ByteString
    , iMAC :: ByteString
    }

instance Show ItemKey where
    show = const "ItemKey <...>"

realize :: ByteArrayAccess b => b -> ByteString
realize = convert

derivedKey :: Profile -> Password -> DerivedKey
derivedKey Profile{..} (Password pass) =
    let bytes = generate (prfHMAC SHA512)
                         (Parameters pIterations 512)
                         pass
                         (rawBytes pSalt)
     in DerivedKey (take 32 bytes) (drop 32 bytes)

masterKey :: (Applicative m, Monad m) => Profile -> DerivedKey -> ResultT m MasterKey
masterKey Profile{pMasterKey=mk} DerivedKey{..} = do
    op    <- opdata mk
    bytes <- opDecrypt dKey op
    let hashed = hash bytes :: Digest SHA512
    if length bytes /= 256
       then failure "Badly sized master key"
       else return $ MasterKey (realize $ view hashed 0  32)
                               (realize $ view hashed 32 32)
                               (oIV op)

itemKey :: (Applicative m, Monad m) => Item -> MasterKey -> ResultT m ItemKey
itemKey Item{..} MasterKey{..} = do
    let raw  = rawBytes iEncKey

    let iv   = view raw 0  16
    let dat  = view raw 16 64
    let mac  = view raw 64 32

    ctx <- liftCrypto $ cipherInit mKey
    iv' <- liftMaybe "Could not create IV" $ makeIV iv
    let bytes = cbcDecrypt (ctx :: AES256) iv' (realize dat)
    return $ ItemKey (realize $ view bytes 0 32) (realize $ view bytes 32 32)

itemData :: (Applicative m, Monad m) => Item -> ItemKey -> ResultT m ByteString
itemData Item{..} ItemKey{..} = opDecrypt iKey =<< opdata iDetails
