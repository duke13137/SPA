# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Development

This project uses GHC 9.10 directly (no .cabal or stack.yaml). Dependencies are managed via `cabal install --lib` into a GHC package environment.

| Command    | Purpose                                          |
|------------|--------------------------------------------------|
| `make dev` | Start dev server with ghciwatch (hot reload, port 8080) |
| `make exe` | Build threaded binary to `.build/hs`             |
| `make env` | Install all dependencies into GHC environment    |
| `make hie` | Rebuild HIE database for IDE support             |
| `make pg`  | Start PostgreSQL in Docker (port 5432)           |

### Running Tests

Tests run inside GHCi (via `make dev` or `ghci-9.10`), using eval comments:

```haskell
-- $> tasty testRoute    -- HTTP route tests (no DB needed)
-- $> testDB             -- Database tests (requires PostgreSQL)
```

## Architecture

A Haskell web app combining two UI approaches on a single Warp server (port 8080):

- **Twain routes** (`/`, `/hello/:name`) — traditional request/response with HTMX for dynamic updates, rendered via `[hsx|...|]` quasiquoter (Lucid + IHP.HSX)
- **Hyperbole app** (`/hyper/*`) — reactive server-side components with typed actions, mounted via `Wai.mount`

Routing is unified in `Main.app` using `Wai.mapUrls` to dispatch between Twain middleware and the Hyperbole live app.

### Key Modules

- **`Main`** — Server startup (with `Rapid` for hot reload), routes, Twain handlers, Hyperbole views
- **`Prelude`** — Custom prelude: re-exports Relude, io-classes (MonadSTM/MonadAsync/etc.), and strict-wrapper
- **`Database`** — Hasql connection/pool setup; reads `PGHOST`, `PGPORT`, `PGDATABASE`, `PGUSER`, `PGPASSWORD` env vars
- **`Htmx`** — Configures the `hsx` quasiquoter with HTMX attributes and custom tag names
- **`Test`** — Tasty test trees; uses tasty-wai for HTTP tests and hasql-th for DB tests

### Conventions

- GHC2024 language standard (set in `.ghci`), plus `OverloadedRecordDot`, `OverloadedLabels`, `StrictData`
- `NoImplicitPrelude` — all modules import the custom `Prelude`
- Source lives in `src/`, set via `:set -isrc` in `.ghci` and `-isrc` in Makefile
- `[hsx|...|]` quasiquoter for HTML (not `[shamlet]` or raw Lucid combinators)
- Type-safe SQL via `hasql-th` quasiquoters (`[singletonStatement|...|]`)
- `-fdefer-type-errors` is on in GHCi for incremental development
