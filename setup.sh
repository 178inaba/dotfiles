#!/bin/sh

# dotfiles path
DF=~/.dotfiles

[ ! -e ~/.bashrc.local ] && mv -v ~/.bashrc ~/.bashrc.local

ln -fsv $DF/.bashrc ~/.bashrc

# load
. ~/.bashrc
