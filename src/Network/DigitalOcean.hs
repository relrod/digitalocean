{-# LANGUAGE OverloadedStrings, RankNTypes, ScopedTypeVariables #-}
-- BSD-3, Ricky Elrod <ricky@elrod.me>
-- BSD-3, Maxwell Swadling <maxwellswadling@gmail.com>

-- TODO:
-- - Dryrun mode
module Network.DigitalOcean (
  Authentication (..),
  DOResponse (..),
  Droplet (..), droplets,
  Region (..), regions,
  Size (..), sizes,
  Image (..), images,
  SSH (..), ssh_keys,
  NewDropletRequest (..), newDroplet,
  PackedDroplet (..), packDroplets
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
import Data.Maybe
import Data.List (intercalate, concat, find)

data Authentication = Authentication { clientId :: String, apiKey ::  String } deriving (Show)

data Droplet = Droplet {
  dropletId :: Integer,
  name :: String,
  imageId :: Integer,
  sizeId :: Integer,
  regionId :: Integer,
  backupsActive :: Bool,
  ipAddress :: String,
  locked :: Bool,
  status' :: String,
  createdAt :: String
} deriving (Show)

data Region = Region {
  rId :: Integer,
  rName :: String
} deriving (Show)

data Size = Size {
  sId :: Integer,
  sName :: String,
  sMemory :: Integer,
  sCpu :: Integer,
  sDisk :: Integer,
  sCostHour :: Float,
  sCostMonth :: String -- Yeah, it's a string.
} deriving (Show)

data Image = Image {
  iImageId :: Integer,
  iImageName :: String,
  iImageDistribution :: String
} deriving (Show)

data SSH = SSH {
  sshId :: Integer,
  sshName :: String
} deriving (Show)

data NewDropletRequest = NewDropletRequest {
  ndName :: String,
  ndSizeId :: Integer,
  ndImageId :: Integer,
  ndRegionId :: Integer,
  ndSSHKeys :: [Integer]
} deriving (Show)

data NewDroplet = NewDroplet {
  ndId :: Integer,
  ndEventId :: Integer
} deriving (Show)

data DOResponse a = DOResponse {
  rResponseStatus :: String,
  rResponseObjects :: a
} deriving (Show)

type DropletsResponse = DOResponse [Droplet]
type NewDropletResponse = DOResponse NewDroplet
type RegionsResponse = DOResponse [Region]
type SizesResponse = DOResponse [Size]
type ImagesResponse = DOResponse [Image]
type SSHsResponse = DOResponse [SSH]

-- a droplet with objects (joined on id)
data PackedDroplet = PackedDroplet {
  pDropletId :: Integer,
  pName :: String,
  pImage :: Image,
  pSize :: Size,
  pRegion :: Region,
  pBackupsActive :: Bool,
  pIpAddress :: String,
  pLocked :: Bool,
  pStatus' :: String,
  pCreatedAt :: String
} deriving (Show)

-- could do lenses...
packDroplets :: [Size] -> [Region] -> [SSH] -> [Image] -> [Droplet] -> [PackedDroplet]
packDroplets _ _ _ _ [] = []
packDroplets s r k i ((Droplet idx n im si re b ip l st c):xs) =
  PackedDroplet idx n (getImage im i) (getSize si s) (getRegion re r) b ip l st c : packDroplets s r k i xs
  where
    -- safe by construction? :P
    getImage i  = fromJust . find (\(Image x _ _) -> x == i)
    getSize i   = fromJust . find (\(Size x _ _ _ _ _ _) -> x == i)
    getRegion i = fromJust . find (\(Region x _) -> x == i)

-- request an array of objects
request :: (FromJSON a, DOResp a) => String -> String -> Authentication -> (MonadIO m) => m (Maybe (DOResponse [a]))
request url p x = liftM decode $ simpleHttp $ constructURL ("/" <> url) x p
-- request a single object
requestObject :: (FromJSON a, DOResp a) => String -> String -> Authentication -> (MonadIO m) => m (Maybe (DOResponse a))
requestObject url p x = liftM decode $ simpleHttp $ constructURL ("/" <> url) x p

class DOResp a where
  primaryKey :: a -> Text
instance DOResp a => DOResp [a] where
  -- the request / requestObject ensure you can't lookup key on an array
  primaryKey _ = primaryKey (undefined :: a)
instance DOResp Droplet where
  primaryKey _ = "droplets"
instance DOResp NewDroplet where
  primaryKey _ = "droplet"
instance DOResp Region where
  primaryKey _ = "regions"
instance DOResp Size where
  primaryKey _ = "sizes"
instance DOResp Image where
  primaryKey _ = "images"
instance DOResp SSH where
  primaryKey _ = "ssh_keys"

class MkParams a where
  mkParams :: a -> String

instance MkParams NewDropletRequest where
  mkParams (NewDropletRequest n s i r ssh) = concat (map (\(k, v) -> "&" ++ k ++ "=" ++ v) vals)
    where
      vals = [
          ("name", urlEncode n)
        , ("size_id", show s)
        , ("image_id", show i)
        , ("region_id", show r)
        , ("ssh_key_ids", intercalate "," (map show ssh))
        ] 

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

instance FromJSON NewDroplet where
  parseJSON (Object v) =
    NewDroplet <$>
    (v .: "id") <*>
    (v .: "event_id")

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

instance FromJSON Image where
  parseJSON (Object v) =
    Image <$>
    (v .: "id") <*>
    (v .: "name") <*>
    (v .: "distribution")

instance FromJSON SSH where
  parseJSON (Object v) =
    SSH <$>
    (v .: "id") <*>
    (v .: "name")

-- The API url
url :: String
url = "https://api.digitalocean.com"

-- QueryString that contains authentication information (client ID and API key)
authQS :: Authentication -> String
authQS a = "client_id=" ++ urlEncode (clientId a) ++ "&api_key=" ++ urlEncode (apiKey a)

constructURL :: String -> Authentication -> String -> String
constructURL a b p = url ++ a ++ "?" ++ authQS b ++ p

droplets :: Authentication -> (MonadIO m) => m (Maybe DropletsResponse)
droplets = request "droplets" ""

newDroplet :: NewDropletRequest -> Authentication -> (MonadIO m) => m (Maybe NewDropletResponse)
newDroplet r = requestObject "droplets/new" (mkParams r)

regions :: Authentication -> (MonadIO m) => m (Maybe RegionsResponse)
regions = request "regions" ""

sizes :: Authentication -> (MonadIO m) => m (Maybe SizesResponse)
sizes = request "sizes" ""

images :: Authentication -> (MonadIO m) => m (Maybe ImagesResponse)
images = request "images" ""

ssh_keys :: Authentication -> (MonadIO m) => m (Maybe SSHsResponse)
ssh_keys = request "ssh_keys" ""
