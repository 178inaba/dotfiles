#!/bin/bash

# link files
FILES=(
	atom
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

# emacs version
INSTALL_EMACS_VER=25.1
