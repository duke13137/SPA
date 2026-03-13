# Haskell Project Guide

## Architecture

A Haskell web app combining two UI approaches on a single Warp server (port 8080):

- Hypermedia-Driven Web Applications With HTMX
  <https://hypermedia.systems/part/htmx/>
  <https://four.htmx.org/reference/>
  <https://picocss.com/docs>
- **Twain routes** (`/`, `/hello/:name`) — traditional request/response with HTMX for dynamic updates, rendered via `[hsx|...|]` quasiquoter (Lucid + IHP.HSX)
- **Servant API** (`/api/*`) — <https://github.com/haskell-servant/servant>

## Environment

This folder uses a Makefile-based workflow. The `env` target installs a local
package environment with GHC and the following libraries:

- base
- template-haskell
- text
- relude
- containers
- stm-containers
- aeson
- optics
- aeson-optics
- http-media
- pretty-show
- strict-wrapper
- breakpoint
- bluefin
- co-log
- io-classes
- io-sim
- hasql
- hasql-th
- hasql-pool
- hasql-transaction
- servant
- servant-client
- servant-server
- servant-quickcheck
- rapid
- twain
- wai
- wai-extra
- warp
- tasty
- tasty-hunit
- tasty-wai

## GHCi Commands

This project loads both `~/.ghci` and `.ghci`. Useful commands defined there
include:

- `:hoogle <query>` runs hoogle against the local `.haskell.hoo` database.
- `:hdoc <query>` shows hoogle `--info` output for a query.

## Check and fix errors in ghcid.txt when editing code

- See <https://errors.haskell.org/index.html> for error detail.
- Search type `hoogle --database .haskell.hoo -q <type>`
- Read doc `hoogle --database .haskell.hoo -q --info <name>`
- Fix problems `hlint --git`
- DO NOT use cabal command
