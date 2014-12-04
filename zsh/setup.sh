#!/bin/sh

cd $(dirname $0)

set -x

[ ! -s $HOME/.oh-my-zsh ] && curl -L http://install.ohmyz.sh | sh
cp -p zsh/zshrc $HOME/.zshrc
