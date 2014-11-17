#!/bin/sh

set -x

# --------------------------------------------------
# @ git
which git
if [ $? -ne 0 ]; then
  exit 1
fi

LOCALX=$HOME/.dotfiles
[ ! -s $LOCALX ] && git clone https://github.com/t8keru/dotfiles.git $LOCALX
cd $LOCALX && git pull --rebase && cd $HOME
# --------------------------------------------------

# --------------------------------------------------
# @ zsh
[ ! -s $HOME/.oh-my-zsh ] && curl -L http://install.ohmyz.sh | sh
cp -p $LOCALX/zsh/zshrc $HOME/.zshrc

# --------------------------------------------------


# --------------------------------------------------
# @ emacs
# which emacs
# if [ $? -eq 0 ]; then
#   mkdir -p $HOME/.emacs.d
#   cp -p $LOCALX/emacs/* $HOME/.emacs.d/
# 
#   [ ! -s "$HOME/.cask/bin/cask" ] && curl -fsSkL https://raw.github.com/cask/cask/master/go | python
#   cd $HOME/.emacs.d
#   $HOME/.cask/bin/cask
# fi
# --------------------------------------------------

# --------------------------------------------------
# @ haskell
which cabal
if [ $? -eq 0 ]; then
  [ ! -s $HOME/.cabal/bin/cabal ] && cabal install cabal-install
  $HOME/.cabal/bin/cabal update
  [ ! -s $HOME/.cabal/bin/ghc-mod ] && $HOME/.cabal/bin/cabal install ghc-mod
  [ ! -s $HOME/.cabal/bin/hoogle ] && $HOME/.cabal/bin/cabal install hoogle
fi
# --------------------------------------------------

# --------------------------------------------------
# @ golang
which go
if [ $? -ne 0 ]; then
  mkdir -p $HOME/src
  cd $HOME/src
  [ ! -s go ] && hg clone -u release https://code.google.com/p/go
  cd go
  ./all.bash

  exec $SHELL

  go get github.com/peco/peco/cmd/peco
  go get github.com/motemen/ghq
fi
# --------------------------------------------------

# --------------------------------------------------
# @ ruby
RB_VER=2.1
if [ ! -s $HOME/.rvm/scripts/rvm ]; then
  curl -sSL https://get.rvm.io | bash
  source $HOME/.rvm/scripts/rvm
  rvm install $RB_VER
  rvm use $RB_VER --default
fi

# --------------------------------------------------
# @ python
PY_VER=2.7.8
[ ! -s $HOME/.pyenv/bin/pyenv ] && curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
eval "$($HOME/.pyenv/bin/pyenv init -)"
[ "$(python -V>/dev/stdout 2>&1)" != "Python $PY_VER" ] && $HOME/.pyenv/bin/pyenv install $PY_VER && $HOME/.pyenv/bin/pyenv rehash && $HOME/.pyenv/bin/pyenv global $PY_VER
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PATH="$PYENV_ROOT/shims/:$PATH"
pip install -r pip/requirements.txt
# --------------------------------------------------

# --------------------------------------------------
# @ node.js
NODE_VER=0.10.33
if [ ! -s $HOME/.nvm/nvm.sh ]; then
  curl https://raw.githubusercontent.com/creationix/nvm/v0.18.0/install.sh | bash
  source $HOME/.nvm/nvm.sh
  nvm install $NODE_VER
  nvm use $NODE_VER
  nvm alias default $NODE_VER
fi
# --------------------------------------------------

# --------------------------------------------------
# @ vim
which vim
if [ $? -eq 0 ]; then
  mkdir -p $HOME/.vim/bundle
  cp -p $LOCALX/vim/bundle.yml $HOME/.vim/
  cp -p $LOCALX/vim/vimrc $HOME/.vimrc
  cp -p $LOCALX/vim/gvimrc $HOME/.gvimrc

  [ ! -s $HOME/.vim/bundle/neobundle.vim ] && git clone https://github.com/Shougo/neobundle.vim $HOME/.vim/bundle/neobundle.vim
  $HOME/.vim/bundle/neobundle.vim/bin/neoinstall

fi
# --------------------------------------------------

