#!/bin/sh

case $(uname) in
"FreeBSD") pkg install -y gcc49 openjdk8 ruby python mercurial git vim gvim hs-haskell-platform bash gmake ;;
*) ;;
esac
