#!/bin/sh

set -e
set -x

cd $(dirname $0)

GOROOT=$HOME/local/go; export GOROOT
# GOOS=freebsd;          export GOOS
GOOS=linux;            export GOOS
# GOARCH=amd64;          export GOARCH
GOARCH=386;            export GOARCH
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
$GO get -u golang.org/x/tools/cmd/godoc
$GO get -u golang.org/x/tools/cmd/goimports
