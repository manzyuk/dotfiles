#!/bin/sh

for p in `ghc-pkg check $* 2>&1  | grep problems | awk '{print $6}' | sed -e 's/:$//'`
do
    echo unregistering $p; ghc-pkg $* unregister $p
    cabal install $p
done
