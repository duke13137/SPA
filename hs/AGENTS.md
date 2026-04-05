# Haskell Project — Agent Guide

## Hard Rules

- **DO NOT run `cabal`, `stack`, or `make`**
- **DO NOT edit files in `.build/`**
- After every file edit, read `ghcid.txt` before proceeding

## Build & Error Feedback

`ghciwatch` runs in the background and writes live errors to `ghcid.txt`.

```bash
cat ghcid.txt          # check compile errors / test failures
```

GHC error codes → <https://errors.haskell.org/index.html>

## Hole-Driven Development

Write type signatures first; use `_` for unknowns. GHCid surfaces typed-hole
suggestions in `ghcid.txt` immediately on save. Fill holes incrementally.

## REPL-Driven Development

1. Find the `ghci` pane in the tmux session
2. Send commands/snippets via tmux (`:reload`, `:type`, `:browse! *Module`)
3. Use eval comments to run expressions: `-- $> expr`

```bash
# Explore a module
ghci -e ':browse! Prelude'
ghci -e ':browse! Data.Map.Strict'

# Docs by name or type
ghci -e ':hoogle traverse'
ghci -e ':hoogle a -> Maybe b'
ghci -e ':hdoc traverse'
```

## Testing

```
-- In GHCi (or as eval comment):
tasty testRoute
tasty testDB
```
