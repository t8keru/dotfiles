#!/bin/sh
# vim: set ts=2 sw=2 sts=8 et:

set -ex

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
# @ zsh
zsh/setup.sh
exec $SHELL

# --------------------------------------------------
# @ emacs
emacs/setup.sh
exec $SHELL

# --------------------------------------------------
# @ haskell
haskell/setup.sh
exec $SHELL

# --------------------------------------------------
# @ golang
golang/setup.sh
exec $SHELL

# --------------------------------------------------
# @ ruby
ruby/setup.sh 2.1
exec $SHELL

# --------------------------------------------------
# @ python
python/setup.sh 2.7.8
exec $SHELL

# --------------------------------------------------
# @ node.js
nodejs/setup.sh 0.10.33
exec $SHELL

# --------------------------------------------------
# @ vim
vim/setup.sh
exec $SHELL

# --------------------------------------------------
# @ nvim
nvim/setup.sh
exec $SHELL

# --------------------------------------------------
# @ git
git/setup.sh
exec $SHELL
