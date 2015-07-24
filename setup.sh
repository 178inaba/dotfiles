#!/bin/sh

# package manager for each OS
case $OSTYPE in
	# mac
	darwin*) PKGMGR='brew';;
	# linux
	linux*)
		if type yum >/dev/null 2>&1; then
			PKGMGR='sudo yum'
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

# download
. ./dl.sh

# install
. ./install.sh

[ ! -e ~/.bashrc.local ] && mv -v ~/.bashrc ~/.bashrc.local

ln -fsv $DF/.bashrc ~/.bashrc

# reload shell
cd $CURPWD
exec -l $SHELL
