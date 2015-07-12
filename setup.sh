#!/bin/sh

# dotfiles path
DF=~/.dotfiles

if [ ! -e ~/.bashrc.local ]; then
	mv -v ~/.bashrc ~/.bashrc.local
fi

ln -fsv $DF/.bashrc ~/.bashrc

. ~/.bashrc
