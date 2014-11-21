#!/bin/sh

cd $(dirname $0)

VER=$1

if [ ! -s $HOME/.nvm/nvm.sh ]; then
  curl https://raw.githubusercontent.com/creationix/nvm/v0.18.0/install.sh | bash
  source $HOME/.nvm/nvm.sh
  nvm install $VER
  nvm use $VER
  nvm alias default $VER
fi
