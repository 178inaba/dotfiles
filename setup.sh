#!/bin/sh

# dotfiles path
DF=~/.dotfiles

# download
sh $DF/dl.sh

# install
sh $DF/install.sh

[ ! -e ~/.bashrc.local ] && mv -v ~/.bashrc ~/.bashrc.local

ln -fsv $DF/.bashrc ~/.bashrc

# load
. ~/.bashrc
