#!/bin/sh

cd $(dirname $0)

which vim
if [ $? -eq 0 ]; then
  mkdir -p $HOME/.vim/bundle
  cp -p vim/bundle.yml $HOME/.vim/
  cp -p vim/vimrc $HOME/.vimrc
  cp -p vim/gvimrc $HOME/.gvimrc

  [ ! -s $HOME/.vim/bundle/neobundle.vim ] && git clone https://github.com/Shougo/neobundle.vim $HOME/.vim/bundle/neobundle.vim
  $HOME/.vim/bundle/neobundle.vim/bin/neoinstall

fi
