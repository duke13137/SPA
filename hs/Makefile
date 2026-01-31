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
	cabal install -w ghc-$(GHC) --allow-newer --enable-documentation --package-env . \
		--lib base template-haskell relude text containers stm stm-containers \
					aeson optics aeson-optics pretty-show strict-wrapper time \
					bluefin co-log io-classes io-sim streamly-core wai wai-extra warp \
					hasql hasql-th hasql-implicits hasql-pool hasql-transaction \
					atomic-css cookie http-api-data http-types hyperbole twain \
					auto-split breakpoint rapid tasty tasty-hunit tasty-wai

hie:
	hiedb-$(GHC) -D .hiedb index .build

pg:
	docker run --name postgres \
		-e POSTGRES_PASSWORD=$(shell cat /var/run/pgpasswd) \
		-p 5432:5432 \
		-d postgres
