#!/bin/sh

case $(uname) in
"FreeBSD")
  pkg install -y gcc5 openjdk8 ruby python mercurial git vim gvim hs-haskell-platform bash gmake
  X=`which gcc5`
  [ -s "$x" ] && ln -s $x /usr/local/bin/gcc
  ;;
*) ;;
esac
