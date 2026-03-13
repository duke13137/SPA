SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

GHC  ?= 9.10
MAIN ?= Main
EXE  := $(shell basename $(MAIN) .Main)

.PHONY: dev exe env hie pg

dev:
	ghciwatch --clear --no-interrupt-reloads \
		--command ghci-$(GHC) \
		--after-reload-shell 'make hie' \
		--error-file ghcid.txt \
		--enable-eval --watch .

exe:
	mkdir -p .build && ln -sf .build .hiefiles && ln -sf .build .hifiles
	ghc-$(GHC) -threaded -isrc -outputdir .build -main-is $(MAIN) -o .build/$(EXE) $(MAIN)

env:
	rm .ghc.environment.*$(GHC)* || true
	cabal install -w ghc-$(GHC) --allow-newer --enable-documentation \
    --package-env . \
    --constraint='wai-app-static < 3.1.8' \
    --lib base template-haskell text relude containers stm-containers \
          aeson optics aeson-optics http-media pretty-show strict-wrapper \
          breakpoint bluefin co-log io-classes io-sim \
          hasql hasql-th hasql-pool hasql-transaction \
          servant servant-client servant-server servant-quickcheck \
          rapid twain wai wai-extra warp tasty tasty-hunit tasty-wai

hie:
	hiedb-$(GHC) -D .hiedb index .build

pg:
	docker run --name postgres \
		-e POSTGRES_PASSWORD=$(PGPASSWORD) \
		-p 5432:5432 \
		-d postgres
