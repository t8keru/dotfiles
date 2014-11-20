#!/bin/sh

case $(uname) in
"FreeBSD")
  pkg install -y gcc5 openjdk8 ruby python node erlang mercurial git vim gvim hs-haskell-platform bash gmake
  ln -s $x /usr/local/bin/gcc
  cabal update
  
  ;;
*) ;;
esac
