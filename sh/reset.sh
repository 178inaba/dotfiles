#!/bin/bash

# delete package sub command for each OS
case $OSTYPE in
	# mac
	darwin*)
		PKGDEL='brew uninstall'
		;;
	# linux
	linux*)
		if type yum >/dev/null 2>&1; then
			PKGDEL='sudo yum -y remove'
		elif type apt-get >/dev/null 2>&1; then
			PKGDEL='apt-get remove'
		fi
		;;
esac

# dotfiles path
DF=~/.dotfiles

# load const
. $DF/sh/const.sh

# delete package
PKGS=(git ${PKGS[@]})
for PKG in ${PKGS[@]}
do
	if type $PKG >/dev/null 2>&1; then
		$PKGDEL $PKG
	fi
done

# local -> origin
cd
for FILE in ${FILES[@]}
do
	if [ -L .$FILE ]; then
		unlink .$FILE
		mv -v .$FILE.local .$FILE
	fi
done

rm -rfv $DF
