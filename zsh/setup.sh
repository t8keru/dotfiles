#!/bin/sh

set -x
set -e

cd $(dirname $0)

[ ! -s $HOME/.oh-my-zsh ] && curl -L http://install.ohmyz.sh | sh
cp -p zshrc $HOME/.zshrc
