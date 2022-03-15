#!/bin/bash

set -eu

if [ -e ~/.vimrc ] && [ ! -L ~/.vimrc ] && [ ! -e ~/.vimrc.local ]; then
  mv -v ~/.vimrc ~/.vimrc.local
fi

ln -fnsv $DF/vimrc ~/.vimrc

vim +PlugInstall +qall
