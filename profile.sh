#!/usr/bin/env bash

rm -f profile*.prof profile*.prof.html
cabal v1-configure --enable-profiling --enable-tests --enable-benchmarks
cabal v1-run profile0
profiteur profile0.prof
firefox profile0.prof.html
