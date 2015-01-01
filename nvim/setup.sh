#!/bin/sh

set -x
set -e

cd $(dirname $0)

which nvim
if [ $? -eq 0 ]; then
  cp -p nvim/nvimrc $HOME/.nvimrc

  X=$HOME/dev/src/github.com/neovim

  mkdir -p $X
  cd $X
  [ ! -s neovim ] && git clone https://github.com/neovim/neovim.git
  cd neovim

  MAKE=make
  which gmake
  [ $? == 0 ] && MAKE=gmake
  echo $MAKE
  $MAKE CMAKE_EXTRA_FLAGS="-DCMAKE_INSTALL_PREFIX:PATH=$HOME/local/neovim" install

fi
