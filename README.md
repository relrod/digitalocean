# digitalocean-hs

A Haskell library for working with the DigitalOcean API.

Released under the 3-clause BSD license.

Implements:

- GET '/droplets'
- GET '/droplets/new'
- GET '/regions'
- GET '/sizes'
- GET '/images'
- GET '/ssh_keys'

## Using

```haskell
let a = Authentication "redacted" "redacted"
x <- droplets a

liftM (map ipAddress . rDroplets) x
-- Just ["198.211.109.123","192.241.204.123"]

fmap (map name . rDroplets) x
-- Just ["myserver01.example.com","myserver02.example.com"]
```
