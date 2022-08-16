#!/usr/bin/env bash

set -e
cd src
stack ghc Main.hs -- -Wall -o ../Sykl
rm *.hi *.o