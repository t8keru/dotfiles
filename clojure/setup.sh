#!/bin/sh

set -x
set -e

cd $(dirname $0)

mkdir -p ~/bin
rm -rf ~/bin/lein
curl -L -o ~/bin/lein https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein

chmod +x ~/bin/lein

mkdir -p ~/.lein
cp -p profiles.clj ~/.lein/profiles.clj


~/bin/lein plugin install gberenfield/lein-nailgun 2.3.1
