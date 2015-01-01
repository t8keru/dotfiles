#!/bin/sh

set -x
set -e

cd $(dirname $0)

which emacs
if [ $? -eq 0 ]; then
  mkdir -p $HOME/.emacs.d
  cp -p Cask $HOME/.emacs.d/
  cp -p init.el $HOME/.emacs.d/

  [ ! -s "$HOME/.cask/bin/cask" ] && curl -fsSkL https://raw.github.com/cask/cask/master/go | python
  cd $HOME/.emacs.d && $HOME/.cask/bin/cask
fi
