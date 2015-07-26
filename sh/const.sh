#!/bin/bash

# link files
FILES=(
    bashrc
    emacs.d
)

# install packages
PKGS=(
    go
    emacs
    tree
)

# overinstall packages
OPKGS=(
    emacs
)

# install option
OPTbrewgo=--with-cc-all
