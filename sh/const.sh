#!/bin/bash

# link files
FILES=(
	bash_profile
	bashrc
	emacs.d
	gitconfig
	vimrc
)

# install packages
PKGS=(
	bash-completion
	go
	emacs
	tree
	wget
)

# overinstall packages
OPKGS=(
	emacs
)

# install option
OPTbrewgo=--with-cc-all

# emacs version
INSTALL_EMACS_VER=24.5
