#!/bin/sh

cd $(dirname $0)

set -x

GOROOT=$HOME/local/go; export GOROOT
GOOS=freebsd;          export GOOS
GOARCH=amd64;          export GOARCH
GOPATH=$HOME/dev;      export GOPATH
GO=$GOROOT/bin/go

if [ ! -s $GO ]; then

  mkdir -p $HOME/local
  cd $HOME/local
  [ ! -s go ] && git clone https://github.com/golang/go.git
  cd go/src && ./all.bash

fi

$GO get -u github.com/peco/peco/cmd/peco
$GO get -u github.com/motemen/ghq
$GO get -u github.com/nsf/gocode
$GO get -u code.google.com/p/go.tools/cmd/godoc
