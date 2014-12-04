#!/bin/sh

set -x

cd $(dirname $0)

[ ! -s $HOME/.oh-my-zsh ] && curl -L http://install.ohmyz.sh | sh
cp -p zsh/zshrc $HOME/.zshrc
