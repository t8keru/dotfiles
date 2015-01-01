#!/bin/sh

set -x
set -e

cd $(dirname $0)

which cabal
if [ $? -eq 0 ]; then
  cabal install cabal-install
  cabal update
  [ ! -s $HOME/.cabal/bin/ghc-mod ] && $HOME/.cabal/bin/cabal install ghc-mod
  [ ! -s $HOME/.cabal/bin/hoogle ] && $HOME/.cabal/bin/cabal install hoogle
  [ ! -s $HOME/.cabal/bin/hlint ] && $HOME/.cabal/bin/cabal install hlint
fi
