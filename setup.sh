#!/bin/bash

# package manager for each OS
case $OSTYPE in
    # mac
    darwin*) PKGMGR='brew';;
    # linux
    linux*)
        if type yum >/dev/null 2>&1; then
            PKGMGRSUDO='sudo'
            PKGMGR='yum'
            PKGMGROPT='-y --enablerepo=epel'
        elif type apt-get >/dev/null 2>&1; then
            PKGMGR='apt-get'
        fi
        ;;
esac

echo "your package manager is $PKGMGR"

if ! type git >/dev/null 2>&1; then
    $PKGMGRSUDO $PKGMGR $PKGMGROPT install git
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
. ./sh/const.sh
for FILE in ${FILES[@]}
do
    if [ -e ~/.$FILE ] && [ ! -L ~/.$FILE ] && [ ! -e ~/.$FILE.local ]; then
        mv -v ~/.$FILE ~/.$FILE.local
    fi
    ln -fnsv $DF/$FILE ~/.$FILE
done

# install
. ./sh/install.sh

# reload shell
cd $CURPWD
exec -l $SHELL
