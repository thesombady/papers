#!/bin/usr/sh
cabal install exe:papers \
  --installdir=$HOME/.lib/bin \
  --overwrite-policy=always \
  --ghc-options="-O2"

mkdir -p ~/.Papers/pdfs
mkdir -p ~/.Papers/bibs
touch -f ~/.Papers/meta.toml
