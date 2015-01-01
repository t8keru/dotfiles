#!/bin/sh

set -x
set -e

cd $(dirname $0)


which vim
if [ $? -eq 0 ]; then
  mkdir -p $HOME/.vim/bundle
  cp -p vimrc $HOME/.vimrc
  cp -p gvimrc $HOME/.gvimrc
  cp -p ctags $HOME/.ctags

  curl https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh | sh

  $HOME/.vim/bundle/neobundle.vim/bin/neoinstall

fi
