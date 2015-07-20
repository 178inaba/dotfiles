#!/bin/sh

# install package
PKG=(go)

for I in ${PKG[@]}
do
	if ! type $I >/dev/null 2>&1; then
		$PKGMGR install $I
	fi
done
