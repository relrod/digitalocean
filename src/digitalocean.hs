{-# LANGUAGE OverloadedStrings, RankNTypes #-}
-- BSD-3, Ricky Elrod <ricky@elrod.me>

module Network.DigitalOcean (
  Authentication (..),
  droplets
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Control.Monad.IO.Class
import Data.Aeson ((.:), (.:?), decode, FromJSON(..), Value(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.HTTP.Conduit
import Network.HTTP.Base (urlEncode)

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

data DropletsResponse = DropletsResponse {
  status :: String,
  rDroplets :: [Droplet]
} deriving (Show)

data RegionsResponse = RegionsResponse {
  rResponseStatus :: String,
  rRegions :: [Region]
} deriving (Show)

data Region = Region {
  rId :: Int,
  rName :: String
} deriving (Show)

instance FromJSON DropletsResponse where
  parseJSON (Object v) =
    DropletsResponse <$>
    (v .: "status") <*>
    (v .: "droplets")

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

instance FromJSON RegionsResponse where
  parseJSON (Object v) =
    RegionsResponse <$>
    (v .: "status") <*>
    (v .: "regions")

instance FromJSON Region where
  parseJSON (Object v) =
    Region <$>
    (v .: "id") <*>
    (v .: "name")

-- The API url
url :: String
url = "https://api.digitalocean.com"

-- QueryString that contains authentication information (client ID and API key)
authQS :: Authentication -> String
authQS a = "client_id=" ++ urlEncode (clientId a) ++ "&api_key=" ++ urlEncode (apiKey a)

constructURL :: String -> Authentication -> String
constructURL a b = url ++ a ++ "?" ++ authQS b

-- GET /droplets
droplets :: Authentication -> (MonadIO m) => m (Maybe DropletsResponse)
droplets a = liftM decode $ simpleHttp $ constructURL "/droplets" a

-- GET /regions
regions :: Authentication -> (MonadIO m) => m (Maybe RegionsResponse)
regions a = liftM decode $ simpleHttp $ constructURL "/regions" a
