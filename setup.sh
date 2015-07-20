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

PWD=`pwd`
echo $PWD

cd
git clone https://github.com/178inaba/.dotfiles.git

# dotfiles path
DF=~/.dotfiles

# download
sh $DF/dl.sh

# install
sh $DF/install.sh

[ ! -e ~/.bashrc.local ] && mv -v ~/.bashrc ~/.bashrc.local

ln -fsv $DF/.bashrc ~/.bashrc

# reload shell
exec $SHELL -l

# git user set
cd $DF

pwd
alias

echo $PWD
cd $PWD
