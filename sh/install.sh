#!/bin/bash

# package install
. ./sh/const.sh
for PKG in ${PKGS[@]}
do
    if ! type $PKG >/dev/null 2>&1; then
	$PKGMGR install $PKG $(eval 'echo $OPT'$MGR$PKG)
    fi
done

# go
export GOPATH=~/work/go
go get -u -v github.com/golang/lint/golint

# emacs
EMVER=(`emacs --version`)
echo "emacs version ${EMVER[2]}"

if ! echo ${EMVER[2]} | grep '24' >/dev/null; then
    mkdir -v $DF/emacs.d/dl
    curl -fsSL -o $DF/emacs.d/dl/package.el http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/package.el?id=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09
fi

emacs -script ~/.emacs.d/setup/setup.el

# git
mkdir -v $DF/git
curl -fsSL -o $DF/git/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -fsSL -o $DF/git/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
