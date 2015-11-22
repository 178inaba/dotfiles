#!/bin/bash

# package install
. ./sh/const.sh
for PKG in ${PKGS[@]}
do
	# remove hyphen(-)
	PKGVAR=$(echo $PKG | sed s/-//g)
	if ! type $PKG >/dev/null 2>&1; then
		$PKGMGRSUDO $PKGMGR $PKGMGROPT install $PKG $(eval 'echo $OPT'$PKGMGR$PKGVAR)
	else
		for OPKG in ${OPKGS[@]}
		do
			if [ $PKG = $OPKG ]; then
				$PKGMGRSUDO $PKGMGR $PKGMGROPT install $PKG $(eval 'echo $OPT'$PKGMGR$PKGVAR)
			fi
		done
	fi
done

# go
export GOPATH=~/work/go
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v code.google.com/p/rog-go/exp/cmd/godef
go get -u -v github.com/golang/lint/golint
go get -u -v github.com/nsf/gocode
go get -u -v github.com/tokuhirom/git-xlsx-textconv

# emacs
NOW_EMACS_VER=(`emacs --version`)
echo "emacs version ${NOW_EMACS_VER[2]}"

if ! echo ${NOW_EMACS_VER[2]} | grep $INSTALL_EMACS_VER >/dev/null; then
	. ./sh/emacs24_$PKGMGR.sh
fi

emacs -script ~/.emacs.d/setup/setup.el

# download
mkdir -pv $DF/download/shellscript $DF/download/bin
curl -fsSL -o $DF/download/shellscript/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -fsSL -o $DF/download/shellscript/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
curl -fsSL -o $DF/download/bin/diff-highlight https://raw.githubusercontent.com/git/git/master/contrib/diff-highlight/diff-highlight
chmod -v a+x $DF/download/bin/diff-highlight
