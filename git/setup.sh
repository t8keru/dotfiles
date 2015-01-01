#!/bin/sh

set -ex

cd $(dirname $0)

git config --global user.name "Takeru Sato"
git config --global user.email "midium.size#h@gmail.com"
git config --global ghq.root $HOME/dev/src
git config --global push.default matching
