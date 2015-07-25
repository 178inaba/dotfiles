#!/bin/bash

# package manager for each OS
case $OSTYPE in
    # mac
    darwin*) PKGMGR='brew';;
    # linux
    linux*)
	if type yum >/dev/null 2>&1; then
	    PKGMGR='sudo yum -y'
	elif type apt-get >/dev/null 2>&1; then
	    PKGMGR='apt-get'
	fi
	;;
esac

echo "your package manager is $PKGMGR"

if ! type git >/dev/null 2>&1; then
    $PKGMGR install git
fi

CURPWD=$(pwd)
cd

# dotfiles path
DF=~/.dotfiles

# clone or pull
if [ -d $DF ]; then
    cd $DF
    git pull
else
    git clone https://github.com/178inaba/dotfiles.git .dotfiles
    cd $DF
fi

# link files
FILES=(bashrc emacs.d)
for FILE in ${FILES[@]}
do
    [ ! -e ~/.$FILE.local ] && mv -v ~/.$FILE ~/.$FILE.local
    ln -fnsv $DF/$FILE ~/.$FILE
done

# download
. ./sh/dl.sh

# install
. ./sh/install.sh

# reload shell
cd $CURPWD
exec -l $SHELL
