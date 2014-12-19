#!/bin/sh

set -ex

cd $(dirname $0)

cp -p gitconfig $HOME/.gitconfig
