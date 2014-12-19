#!/bin/sh

if [ -s cat /etc/issue ]; then
DIST_FILE=/etc/issue
elif[ -s cat /etc/fedora-release ]; then
DIST_FILE=/etc/fedora-release
elif[ -s cat /etc/SuSE-release ]; then
DIST_FILE=/etc/SuSE-release
fi

if [ $DIST_FILE == "" ]; then
DIST=$(uname)
else
DIST=$(cat $DIST_FILE | head -1 | awk "{ print $1 }")
fi

case $DIST in
"FreeBSD")
  freebsd-update fetch
  freebsd-update install

  portsnap fetch
  portsnap extract

  PACKAGE=pkg
  $PACKAGE update -y
  $PACKAGE upgrade -y
  $PACKAGE clean -y
  $PACKAGE install -y gcc5 hs-haskell-platform bash gmake

  [ ! -s /usr/local/bin/gcc ] && ln -s /usr/local/bin/gcc5 /usr/local/bin/gcc

  [ ! -s /usr/local/sbin/portmaster ] && cd /usr/ports/ports-mgmt/portmaster && make install clean

  ;;
"CentOS")
  PACKAGE=yum
  $PACKAGE groupinstall -y "Development Tools"
  $PACKAGE install -y haskell-platform
  ;;
"SUSE")
  PACKAGE=zypper
  $PACKAGE install --type pattern devel_basis
*) ;;
esac

$PACKAGE install -y ninja cmake libtool sha automake pkgconf unzip wget perl openjdk8 ruby python node erlang mercurial git vim 

which gcc
[ $? -ne 0 ] && [ ! -s /usr/local/bin/gcc ] && ln -s /usr/local/bin/gcc5 /usr/local/bin/gcc

cabal update
