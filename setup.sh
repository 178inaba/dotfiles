#!/bin/bash

set -eu

# Install Homebrew on a Mac.
if [[ "${OSTYPE}" =~ darwin* ]] && ! type brew > /dev/null 2>&1; then
  /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

# Install Git.
if ! type git >/dev/null 2>&1; then
  if type brew >/dev/null 2>&1; then
    brew install git
  elif type yum >/dev/null 2>&1; then
    yum install git
  elif type apt-get >/dev/null 2>&1; then
    apt-get install git
  fi
fi

# Save current path.
CURPWD=$(pwd)
cd

# Directory path for dotfiles.
DF=~/.dotfiles

# Clone or pull.
if [ -d $DF ]; then
  cd $DF
  git pull
else
  git clone https://github.com/178inaba/dotfiles.git .dotfiles
  cd $DF
fi

# Link files.
for FILE in 'atom' 'zprofile' 'zshrc' 'emacs.d' 'gitconfig' 'vimrc'
do
  if [ -e ~/.$FILE ] && [ ! -L ~/.$FILE ] && [ ! -e ~/.$FILE.local ]; then
    mv -v ~/.$FILE ~/.$FILE.local
  fi
  ln -fnsv $DF/$FILE ~/.$FILE
done

# Install software, application, tools.
if type brew >/dev/null 2>&1; then
  . ./sh/brew.sh
  . ./sh/apm.sh
fi
. ./sh/vim.sh
. ./sh/emacs.sh
. ./sh/go.sh
. ./sh/composer.sh
. ./sh/download.sh

# Reload shell.
cd $CURPWD
exec -l $SHELL
