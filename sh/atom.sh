#!/bin/bash

set -eu

if [ -e ~/.atom ] && [ ! -L ~/.atom ] && [ ! -e ~/.atom.local ]; then
  mv -v ~/.atom ~/.atom.local
fi

ln -fnsv $DF/atom ~/.atom

apm install package-sync
