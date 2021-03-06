# SQRL Client in Haskell

SQRL is an authentication protocol for web site login. You can find more details in [SQRL](https://www.grc.com/sqrl/sqrl.htm)

## Dependencies
  - Of course you have to install ghc
  - SHA
  - crypto-api
  - http-conduit
  - network

You can install this package by using like
  `cabal install SHA`

## Compile & Test
If you want to compile, `make` is enough
You can go to [APPHB](https://sqrl.apphb.com/), a SQRL server and test this program. When you want to login, run
the `main` binary and then input the sqrl link.

## Structure
- [Dust](/Dust/) is acutally part of [Dust](http://hackage.haskell.org/package/Dust), but I cannot compile the whole package, so I just take what I need.
- [SQRL](/SQRL/) contains all APIs you need to implement a client
- [main.hs](main.hs) is just a toy client which reads the masterkey, get the challenge URL and authenticate the user.
