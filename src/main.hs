{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
-- BSD-3, Maxwell Swadling <maxwellswadling@gmail.com>

module Main (main) where

import Network.DigitalOcean
import System.Environment
import Text.PrettyPrint.Boxes
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.List (transpose)
import Control.Applicative
import Text.Printf
import Control.Concurrent
import Control.Concurrent.MVar

class Boxy a where
  boxit :: a -> [Box]
  title :: a -> [String]
  boxup :: [a] -> IO ()
  boxup = printBox . hsep 1 bottom . transposeWithTitle . map boxit
    where
      transposeWithTitle :: [[Box]] -> [Box]
      transposeWithTitle xs = map (vcat left)
                            $ transpose (map text title'
                              : (replicateWithIndex len'
                                (\i -> text
                                  (replicate (length
                                    (title' !! (len' - i)))
                                    '-')))
                              : xs)
      title' = title (undefined :: a)
      len' = length title'

replicateWithIndex 0 _ = []
replicateWithIndex i f = f i : replicateWithIndex (i - 1) f

t :: (Show a) => a -> Box
t = text . show

tnull Nothing = ""
tnull (Just x) = text x

instance Boxy Droplet where
  boxit (Droplet i n im si re ba ip pip lo st cr) = t i : text n : t im : t si : t re : text ip : tnull pip : text st : text (take 10 cr) : []
  title _ = "id" : "name" : "image_id" : "size_id" : "region_id" : "ip_address" : "private_ip_address" : "status" : "created_at" : []
instance Boxy Size where
  boxit (Size i n m c d ch cm) = t i : text n : t m : t c : t d : text (printf "%.4f" ch) : text cm : []
  title _ = "id" : "name" : "memory" : "cpu" : "disk" : "cost/hr" : "cost/month" : []
instance Boxy Region where
  boxit (Region i n) = t i : text n : []
  title _ = "id" : "name" : []
instance Boxy Image where
  boxit (Image i n d _ _) = t i : text n : text d : []
  title _ = "id" : "name" : "distribution" : []
instance Boxy SSH where
  boxit (SSH i n) = t i : text n : []
  title _ = "id" : "name" : []
instance Boxy PackedDroplet where
  boxit (PackedDroplet i n (Image _ imgn _ _ _) (Size _ sn _ _ _ _ _) (Region _ rn) _ ip pip _ st _) = t i : text n : text ip : tnull pip : text st : text sn : text imgn : text rn : []
  title _ = "id" : "name" : "ip" : "privateip" : "status" : "size" : "image" : "region" : []

main = getArgs >>= \args -> case args of
  ["provision", n, s, i, r, sh, p, b] -> provision n s i r sh p b
  ["--version"] -> putStrLn "version 1.0"
  []            -> report
  ["status"]    -> reportStatus
  _             -> putStrLn "USAGE: (provision)\n\n  provision name size_id image_id region_id ssh_key_id private_networking backups\n  example: provision boxname 66 962304 3 [12345] true false\n"

auth = do
  cid <- getEnv "DIGITAL_OCEAN_CLIENT_ID"
  api <- getEnv "DIGITAL_OCEAN_API_KEY"
  return $ Authentication cid api

provision n s i r sh p b = do
  req <- return $ NewDropletRequest n (read s) (read i) (read r) (read sh) (read p) (read b)
  print req
  a <- auth
  resp <- newDroplet req a
  print resp

report = do
  a <- auth
  let g f = f a >>= \x -> case fmap rResponseObjects x of
        Nothing -> error "network error"
        (Just x') -> boxup x'
      wT n x = putStrLn n >> x >> putStrLn ""
  wT "Droplets" $ g droplets
  wT "Sizes"    $ g sizes
  wT "Regions"  $ g regions
  wT "SSH Keys" $ g sshKeys
  wT "Images"   $ g images

reportStatus = do
  a <- auth
  let g f = f a >>= \x -> case fmap rResponseObjects x of
        Nothing -> error "network error"
        (Just x') -> return x'
  -- do these in parallel
  s <- newEmptyMVar
  forkIO $ g sizes >>= putMVar s
  r <- newEmptyMVar
  forkIO $ g regions >>= putMVar r
  k <- newEmptyMVar
  forkIO $ g sshKeys >>= putMVar k
  i <- newEmptyMVar
  forkIO $ g images >>= putMVar i
  d <- newEmptyMVar
  forkIO $ g droplets >>= putMVar d
  p <- packDroplets <$> takeMVar s <*> takeMVar r <*> takeMVar k <*> takeMVar i <*> takeMVar d
  boxup p
