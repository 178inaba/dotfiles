#!/bin/bash

set -eu

if [ -e ~/.emacs.d ] && [ ! -L ~/.emacs.d ] && [ ! -e ~/.emacs.d.local ]; then
  mv -v ~/.emacs.d ~/.emacs.d.local
fi

ln -fnsv $DF/emacs.d ~/.emacs.d

emacs -script $DF/emacs.d/setup/setup.el
