#!/bin/sh
set -e

mkdir -p out
cabal build && cabal exec example-01 && zip out.zip out/*
