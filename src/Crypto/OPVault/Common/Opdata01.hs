{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault.Common.Opdata01
    (Opdata01(..), opdata) where

import Prelude hiding (drop, length, take)
import Control.Applicative ((<$>))
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString, drop, length, take, unpack)

import Crypto.OPVault.Common.Base64
import Crypto.OPVault.Common.ResultT

data Opdata01 = Opdata01
    { oIV   :: ByteString
    , oData :: ByteString
    , oMAC  :: ByteString
    }

instance Show Opdata01 where
    show = const "Opdata01 <...>"

opdata :: Monad m => Base64 -> ResultT m Opdata01
opdata b64 = liftEither $ A.parseOnly opdataParser $ rawBytes b64

opdataParser :: A.Parser Opdata01
opdataParser = do
    _    <- A.string "opdata01"
    len  <- littleEndian <$> A.take 8
    iv   <- A.take 16
    padding len
    body <- A.take len
    mac  <- A.take 32
    A.endOfInput
    return $ Opdata01 iv body mac


littleEndian :: ByteString -> Int
littleEndian =
    let pos = fmap (256^) [0..]
     in sum . zipWith (*) pos . fmap fromIntegral . unpack

padding :: Int -> A.Parser ()
padding len =
    let r = len `mod` aesBlockSize
     in do if r==0
              then A.take aesBlockSize
              else A.take r
           return ()

aesBlockSize :: Int
aesBlockSize = 16
