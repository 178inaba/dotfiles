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

# package installation of each package manager
case $PKGMGR in
    brew)
        $PKGMGR install homebrew/php/composer
        $PKGMGR cask install atom iterm2
        apm install package-sync
        ;;
esac

# go
go get -u -v golang.org/x/tools/cmd/goimports
go get -u -v golang.org/x/tools/cmd/gopls
go get -u -v github.com/rogpeppe/godef
go get -u -v github.com/golang/lint/golint
go get -u -v github.com/nsf/gocode
go get -u -v github.com/tokuhirom/git-xlsx-textconv
go get -u -v github.com/jingweno/ccat

# composer(php)
composer global require friendsofphp/php-cs-fixer

# emacs
NOW_EMACS_VER=(`emacs --version`)
echo "emacs version ${NOW_EMACS_VER[2]}"

if ! echo ${NOW_EMACS_VER[2]} | grep $INSTALL_EMACS_VER >/dev/null; then
    . ./sh/emacs_$PKGMGR.sh
fi

emacs -script ~/.emacs.d/setup/setup.el

# download
mkdir -pv $DF/dl/{bin,emacs,sh}
curl -fsSL -o $DF/dl/sh/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -fsSL -o $DF/dl/sh/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
curl -fsSL -o $DF/dl/emacs/sgml-mode-patch.el https://gist.githubusercontent.com/178inaba/b143a6f4621e0fcb240cf27e07312eef/raw/db49ed9caaafcb08f16a0389787a1991cbd70727/sgml-mode-patch.el
curl -fsSL -o $DF/dl/emacs/php-cs-fixer.el https://raw.githubusercontent.com/OVYA/php-cs-fixer/master/php-cs-fixer.el
curl -fsSL -o $DF/dl/bin/diff-so-fancy https://raw.githubusercontent.com/so-fancy/diff-so-fancy/master/third_party/build_fatpack/diff-so-fancy

# download file settings
emacs --batch -f batch-byte-compile $DF/dl/emacs/*.el
chmod -v a+x $DF/dl/bin/diff-so-fancy
