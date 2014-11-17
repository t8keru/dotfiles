#!/bin/sh

name=$(uname)

case $name in
"FreeBSD") pkg install -y gcc49 openjdk8 ruby python mercurial git vim emacs24 hs-haskell-platform bash gmake ;;
*) ;;
esac
