# digitalocean-hs

A Haskell library for working with the DigitalOcean API.

Released under the 3-clause BSD license.

This is probably horrible and you probably shouldn't use it. It's one of my
first Haskell projects and is being worked on only to further my understanding
of Haskell. You've been warned. :)

## Using

```haskell
let a = Authentication "redacted" "redacted"
x <- droplets a

liftM (map ipAddress) $ liftM rDroplets x
-- Just ["198.211.109.123","192.241.204.123"]

fmap (map name) $ fmap rDroplets x
-- Just ["myserver01.example.com","myserver02.example.com"]
```
