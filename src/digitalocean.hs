{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
-- BSD-3, Ricky Elrod <ricky@elrod.me>
-- BSD-3, Maxwell Swadling <maxwellswadling@gmail.com>

module Network.DigitalOcean (
  Authentication (..),
  Droplet (..), droplets,
  Region (..), regions,
  Size (..), sizes
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Data.Text (Text, unpack)
import Control.Monad.IO.Class
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Conduit
import Data.Monoid
import Network.HTTP.Base (urlEncode)
import Data.Aeson.Types (Parser)
import Debug.Trace

data Authentication = Authentication { clientId :: String, apiKey ::  String } deriving (Show)

data Droplet = Droplet {
  dropletId :: Int,
  name :: String,
  imageId :: Int,
  sizeId :: Int,
  regionId :: Int,
  backupsActive :: Bool,
  ipAddress :: String,
  locked :: Bool,
  status' :: String,
  createdAt :: String
} deriving (Show)

data Region = Region {
  rId :: Int,
  rName :: String
} deriving (Show)

data Size = Size {
  sId :: Int,
  sName :: String,
  sMemory :: Int,
  sCpu :: Int,
  sDisk :: Int,
  sCostHour :: Int,
  sCostMonth :: String -- Yeah, it's a string.
} deriving (Show)

data DOResponse a = DOResponse {
  rResponseStatus :: String,
  rResponseObjects :: [a]
} deriving (Show)

type DropletsResponse = DOResponse Droplet
type RegionsResponse = DOResponse Region
type SizesResponse = DOResponse Size

class DOResp a where
  primaryKey :: a -> Text
  request :: FromJSON a => Authentication -> (MonadIO m) => m (Maybe (DOResponse a))
  request x = liftM decode $ simpleHttp $ constructURL ("/" <> unpack (primaryKey (undefined :: a)) <> "/") x

instance DOResp Region where
  primaryKey _ = "regions"
instance DOResp Droplet where
  primaryKey _ = "droplets"
instance DOResp Size where
  primaryKey _ = "sizes"

instance (DOResp a, FromJSON a) => FromJSON (DOResponse a) where
  parseJSON (Object v) =
    DOResponse <$>
    (v .: "status") <*>
    (v .: primaryKey (undefined :: a))

instance FromJSON Droplet where
  parseJSON (Object v) =
    Droplet <$>
    (v .: "id") <*>
    (v .: "name") <*>
    (v .: "image_id") <*>
    (v .: "size_id") <*>
    (v .: "region_id") <*>
    (v .: "backups_active") <*>
    (v .: "ip_address") <*>
    (v .: "locked") <*>
    (v .: "status") <*>
    (v .: "created_at")

instance FromJSON Region where
  parseJSON (Object v) =
    Region <$>
    (v .: "id") <*>
    (v .: "name")

instance FromJSON Size where
  parseJSON (Object v) =
    Size <$>
    (v .: "id") <*>
    (v .: "name") <*>
    (v .: "memory") <*>
    (v .: "cpu") <*>
    (v .: "disk") <*>
    (v .: "cost_per_hour") <*>
    (v .: "cost_per_month")

-- The API url
url :: String
url = "https://api.digitalocean.com"

-- QueryString that contains authentication information (client ID and API key)
authQS :: Authentication -> String
authQS a = "client_id=" ++ urlEncode (clientId a) ++ "&api_key=" ++ urlEncode (apiKey a)

constructURL :: String -> Authentication -> String
constructURL a b = url ++ a ++ "?" ++ authQS b

droplets :: Authentication -> (MonadIO m) => m (Maybe DropletsResponse)
droplets = request

regions :: Authentication -> (MonadIO m) => m (Maybe RegionsResponse)
regions = request

sizes :: Authentication -> (MonadIO m) => m (Maybe SizesResponse)
sizes = request