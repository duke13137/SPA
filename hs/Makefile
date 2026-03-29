SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c

GHC  ?= 9.12
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
		--package-env . --lib \
		base template-haskell containers optics relude text \
		aeson aeson-optics generic-data safe-wild-cards strict-wrapper \
		breakpoint co-log ki io-classes io-sim stm stm-containers \
		hasql hasql-th hasql-pool hasql-transaction http-media \
		ihp-hsx lucid2 servant servant-client servant-server \
		rapid twain wai wai-extra warp tasty tasty-hunit tasty-wai

hie:
	hiedb-$(GHC) -D .hiedb index .build

pg:
	docker run --name postgres \
		-e POSTGRES_PASSWORD=$(PGPASSWORD) \
		-p 5432:5432 \
		-d postgres
