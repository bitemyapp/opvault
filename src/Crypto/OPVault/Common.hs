{-# LANGUAGE OverloadedStrings #-}
module Crypto.OPVault.Common
    ( module Crypto.OPVault.Common
    , module CommonImports
    ) where

import Control.Applicative as CommonImports ((<$>), Applicative(..))
import Control.Monad as CommonImpots (mzero)
import Control.Monad.IO.Class as CommonImports (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Aeson as CommonImports (FromJSON(..), (.:), Value(..), Object)
import Data.ByteString.Char8 as CommonImports (ByteString, pack, unpack)
import Data.Foldable as CommonImports (toList)
import Data.HashMap.Strict as CommonImports (HashMap, insert)
import Data.Text as CommonImports (Text)

import Crypto.OPVault.Common.Base64 as CommonImports
import Crypto.OPVault.Common.JSFix as CommonImports
import Crypto.OPVault.Common.Opdata01 as CommonImports
import Crypto.OPVault.Common.ResultT as CommonImports

import qualified Data.HashMap.Strict as M (lookup)
import qualified Data.Text as T (unpack, intercalate)

io :: MonadIO m => IO a -> m a
io = liftIO

lookup :: Monad m => Text -> HashMap Text v -> ResultT m v
lookup k = liftMaybe err . M.lookup k
    where err = T.unpack $ T.intercalate " "
             [ "Could not find key", k, "in map type." ]
