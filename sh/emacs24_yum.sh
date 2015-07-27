#!/bin/bash

# dependent package installation
sudo yum -y install gcc
sudo yum -y install ncurses-devel

# download
curl -fsSL -o emacs-$EMACS_VER.tar.gz http://ftp.gnu.org/gnu/emacs/emacs-$EMACS_VER.tar.gz
tar -zxvf emacs-$EMACS_VER.tar.gz
cd emacs-$EMACS_VER

# install
./configure
make
sudo make install

# delete
rm -frv emacs-24.5.tar.gz emacs-24.5
