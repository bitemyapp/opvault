{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault.Common.JSFix (fromJSON) where

import Control.Applicative ((<|>), (*>), (<*))
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Char8 (ByteString, pack)
import Data.ByteString.Lazy (fromStrict)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly, endOfInput, string, char, many', many1', satisfy)

import Crypto.OPVault.Common.ResultT (ResultT, failure, liftMaybe)

parseProfile :: Parser ByteString
parseProfile = string "var profile="                *>
               fmap pack (many1' $ satisfy (/=';')) <*
               char ';'

parseOthers :: Parser ByteString
parseOthers = many'  (satisfy (/='('))             *>
              char '('                             *>
              fmap pack (many1' $ satisfy(/=')'))  <*
              string ");"

parseVaultData :: Parser ByteString
parseVaultData = (parseOthers <|> parseProfile) <* endOfInput

fromJSON :: (FromJSON a, Monad m) => ByteString -> ResultT m a
fromJSON bytes =
    case parseOnly parseVaultData bytes of
        Left _      -> failure "Could not sanatize OPVault JSON file"
        Right clean -> liftMaybe "Could not parse OPVault JSON file" .
                       decode $ fromStrict clean
