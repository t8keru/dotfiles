#!/bin/sh

set -x
set -e

cd $(dirname $0)

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"

[ ! -s $PYENV_ROOT/bin/pyenv ] && curl -L https://raw.githubusercontent.com/yyuu/pyenv-installer/master/bin/pyenv-installer | bash
eval "$($PYENV_ROOT/bin/pyenv init -)"

[ "$(python -V>/dev/stdout 2>&1)" != "Python $1" ] && $PYENV_ROOT/bin/pyenv install $1 && $PYENV_ROOT/bin/pyenv rehash && $PYENV_ROOT/bin/pyenv global $1

$PYENV_ROOT/shims/pip install -r requirements.txt
