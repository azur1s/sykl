#!/usr/bin/env bash

cd src
ghc Main.hs -Wall -o ../Sykl
s=$?
rm -f *.hi *.o
exit $s