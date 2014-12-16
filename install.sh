#!/bin/sh

case $(uname) in
"FreeBSD")

  freebsd-update fetch
  freebsd-update install

  portsnap fetch
  portsnap extract

  pkg update -y
  pkg upgrade -y
  pkg clean -y
  pkg install -y ninja cmake libtool sha automake pkgconf unzip wget gcc5 perl openjdk8 ruby python node erlang mercurial git vim hs-haskell-platform bash gmake

  [ ! -s /usr/local/bin/gcc ] && ln -s /usr/local/bin/gcc5 /usr/local/bin/gcc

  cabal update

  [ ! -s /usr/local/sbin/portmaster ] && cd /usr/ports/ports-mgmt/portmaster && make install clean

  ;;
*) ;;
esac
