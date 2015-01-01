#!/bin/sh

set -e
set -x

if [ -s /etc/SuSE-release ]; then
  DIST_FILE=/etc/SuSE-release
elif [ -s /etc/fedora-release ]; then
  DIST_FILE=/etc/fedora-release
elif [ -s /etc/issue ]; then
  DIST_FILE=/etc/issue
fi

if [ "$DIST_FILE" == "" ]; then
  DIST=$(uname)
else
  DIST=$(cat $DIST_FILE | head -1 | awk '{ print $1 }')
fi

PKGM=
PKGS=

case $DIST in
  "FreeBSD")
    freebsd-update fetch
    freebsd-update install

    portsnap fetch
    portsnap extract

    PKGM="pkg -y"
    ${PKGM} update
    ${PKGM} upgrade
    ${PKGM} clean
    PKGS="gcc5 hs-haskell-platform bash gmake sha pkgconf node emacs24"

    [ ! -s /usr/local/bin/gcc ] && ln -s /usr/local/bin/gcc5 /usr/local/bin/gcc

    [ ! -s /usr/local/sbin/portmaster ] && cd /usr/ports/ports-mgmt/portmaster && make install clean

    ;;
  "CentOS")
    PKGM="yum -y"
    ${PKGM} groupinstall "Development Tools"
    PKGS="haskell-platform nodejs emacs"
    ;;
  "openSUSE")
    PKGM="zypper --non-interactive"
    ${PKGM} update
    ${PKGM} dist-upgrade
    ${PKGM} clean
    ${PKGM} install --type pattern devel_basis
    PKGS="haskell-platform nodejs emacs java-1_7_0-openjdk"
    ;;
  *) ;;
esac

PKGS="${PKGS} nginx redis ninja cmake libtool automake unzip wget perl ruby python erlang mercurial git vim"

${PKGM} install ${PKGS}

cabal update
