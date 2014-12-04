#!/bin/sh
# vim: set ts=2 sw=2 sts=8 et:

set -xe

# --------------------------------------------------
# @ git
which git
if [ $? -ne 0 ]; then
  exit 1
fi

ROOT_DIR=$HOME/.dotfiles

[ ! -s $ROOT_DIR ] && git clone https://github.com/t8keru/dotfiles.git $ROOT_DIR
cd $ROOT_DIR && git pull --rebase
# --------------------------------------------------

# --------------------------------------------------
# @ zsh
zsh/setup.sh

# --------------------------------------------------
# @ emacs
emacs/setup.sh

# --------------------------------------------------
# @ haskell
haskell/setup.sh

# --------------------------------------------------
# @ golang
golang/setup.sh

# --------------------------------------------------
# @ ruby
ruby/setup.sh 2.1

# --------------------------------------------------
# @ python
python/setup.sh 2.7.8

# --------------------------------------------------
# @ node.js
nodejs/setup.sh 0.10.33

# --------------------------------------------------
# @ vim
vim/setup.sh

# --------------------------------------------------
# @ nvim
nvim/setup.sh
