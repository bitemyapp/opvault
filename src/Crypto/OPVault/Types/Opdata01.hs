{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Crypto.OPVault.Types.Opdata01
    (Opdata01(..), opdata, opDecrypt) where

import Prelude hiding (drop, length, take)
import Control.Applicative ((<$>))
import qualified Data.Attoparsec.ByteString.Char8 as A (take, endOfInput, parseOnly, string, Parser)
import Data.ByteString (ByteString, drop, length, take, unpack)

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cbcDecrypt, cipherInit, makeIV)

import Crypto.OPVault.Types.Base64
import Crypto.OPVault.Types.ResultT

data Opdata01 = Opdata01
    { oIV     :: ByteString
    , oPadLen :: Int
    , oData   :: ByteString
    , oMAC    :: ByteString
    }

instance Show Opdata01 where
    show = const "Opdata01 <...>"

opdata :: Monad m => Base64 -> ResultT m Opdata01
opdata b64 = liftEither $ A.parseOnly opdataParser $ rawBytes b64

opDecrypt :: Monad m => ByteString -> Opdata01 -> ResultT m ByteString
opDecrypt key Opdata01{..} = do
    ctx <- liftCrypto $ cipherInit key
    iv  <- liftMaybe "Could not create initialization vector." $ makeIV oIV
    return . drop oPadLen $ cbcDecrypt (ctx::AES256) iv oData

opdataParser :: A.Parser Opdata01
opdataParser = do
    _    <- A.string "opdata01"
    len <- littleEndian <$> A.take 8
    iv   <- A.take 16
    body <- A.take $ len + padSize len
    mac  <- A.take 32
    A.endOfInput
    return $ Opdata01 iv (padSize len) body mac

padSize :: Int -> Int
padSize len = aesBlockSize - len `mod` aesBlockSize


littleEndian :: ByteString -> Int
littleEndian =
    let pos = fmap (256^) [0..]
     in sum . zipWith (*) pos . fmap fromIntegral . unpack

aesBlockSize :: Int
aesBlockSize = 16
