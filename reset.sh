#!/bin/bash

cd
rm -rf .dotfiles
mv .bashrc.local .bashrc

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

# delete package
. pkgs.sh
PKGS=(git ${PKGS[@]})
for PKG in ${PKGS[@]}
do
    if type $PKG >/dev/null 2>&1; then
	$PKGDEL $PKG
    fi
done
