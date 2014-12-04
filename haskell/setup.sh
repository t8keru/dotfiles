#!/bin/sh

cd $(dirname $0)

set -x

which cabal
if [ $? -eq 0 ]; then
  [ ! -s $HOME/.cabal/bin/cabal ] && cabal install cabal-install
  $HOME/.cabal/bin/cabal update
  [ ! -s $HOME/.cabal/bin/ghc-mod ] && $HOME/.cabal/bin/cabal install ghc-mod
  [ ! -s $HOME/.cabal/bin/hoogle ] && $HOME/.cabal/bin/cabal install hoogle
fi
