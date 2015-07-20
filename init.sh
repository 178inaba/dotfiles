#!/bin/sh

# package manager for each OS
case $OSTYPE in
	# mac
	darwin*) PKGMGR='brew';;
	# linux
	linux*)
		if type yum >/dev/null 2>&1; then
			PKGMGR='yum'
		elif type apt-get >/dev/null 2>&1; then
			PKGMGR='apt-get'
		fi
		;;
esac

echo "your package manager is $PKGMGR"

$PKGMGR install git

cd
git clone https://github.com/178inaba/.dotfiles.git
sh .dotfiles/setup.sh
