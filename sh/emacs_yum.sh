#!/bin/bash

# Dependent package installation.
sudo yum -y install gcc ncurses-devel

# Download.
curl -fsSL -o emacs-$INSTALL_EMACS_VER.tar.gz http://ftp.gnu.org/gnu/emacs/emacs-$INSTALL_EMACS_VER.tar.gz
tar -zxvf emacs-$INSTALL_EMACS_VER.tar.gz
cd emacs-$INSTALL_EMACS_VER

# Install.
./configure
make
sudo make install

# Remove.
cd ..
rm -frv emacs-$INSTALL_EMACS_VER.tar.gz emacs-$INSTALL_EMACS_VER
