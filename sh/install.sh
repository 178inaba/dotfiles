#!/bin/bash

# package install
. ./sh/const.sh
for PKG in ${PKGS[@]}
do
	if ! type $PKG >/dev/null 2>&1; then
		$PKGMGRSUDO $PKGMGR $PKGMGROPT install $PKG $(eval 'echo $OPT'$PKGMGR$PKG)
	else
		for OPKG in ${OPKGS[@]}
		do
			if [ $PKG = $OPKG ]; then
				$PKGMGRSUDO $PKGMGR $PKGMGROPT install $PKG $(eval 'echo $OPT'$PKGMGR$PKG)
			fi
		done
	fi
done

# go
export GOPATH=~/work/go
go get -u -v code.google.com/p/rog-go/exp/cmd/godef
go get -u -v github.com/golang/lint/golint

# emacs
NOW_EMACS_VER=(`emacs --version`)
echo "emacs version ${NOW_EMACS_VER[2]}"

if ! echo ${NOW_EMACS_VER[2]} | grep $INSTALL_EMACS_VER >/dev/null; then
	. ./sh/emacs24_$PKGMGR.sh
fi

emacs -script ~/.emacs.d/setup/setup.el

# git
mkdir -v $DF/git
curl -fsSL -o $DF/git/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -fsSL -o $DF/git/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
