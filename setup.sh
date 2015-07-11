#!/bin/sh

# dotfiles path
DF=~/.dotfiles

mv -v ~/.bashrc ~/.bashrc.local
ln -sv $DF/.bashrc ~/.bashrc
