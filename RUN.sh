#!/bin/sh
set -e

mkdir -p out
cabal build
rm -f out/* out.zip
cabal exec example-01
zip out.zip out/*
