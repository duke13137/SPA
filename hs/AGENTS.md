# Haskell Project Guide

## Build & Development

- This project uses GHC directly (no .cabal or stack.yaml).
- ghci loads `~/.ghci` and `.ghci`.
- **DO NOT run cabal or stack**.

## Hole Driven Development

- Watch ./ghcid.txt for compile errors after each code editing
- Search <https://errors.haskell.org/index.html> for GHC-##### index
- Browse module with `ghci -e ':browse! \*<module>'`
- Read doc `hoogle --database .haskell.hoo -q --info <name>`
