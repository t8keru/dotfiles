#!/bin/sh

cd $(dirname $0)

set -ex

which cabal
if [ $? -eq 0 ]; then
  cabal install cabal-install
  cabal update
  [ ! -s $HOME/.cabal/bin/ghc-mod ] && $HOME/.cabal/bin/cabal install ghc-mod
  [ ! -s $HOME/.cabal/bin/hoogle ] && $HOME/.cabal/bin/cabal install hoogle
fi
