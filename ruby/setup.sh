#!/bin/sh

cd $(dirname $0)

set -x

VER=$1

if [ ! -s $HOME/.rvm/scripts/rvm ]; then
  curl -sSL https://get.rvm.io | bash
  source $HOME/.rvm/scripts/rvm
  rvm install $VER
  rvm use $VER --default
fi
