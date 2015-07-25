#!/bin/bash

# install packages
PKGS=(go)

for PKG in ${PKGS[@]}
do
	if ! type $PKG >/dev/null 2>&1; then
		$PKGMGR install $PKG
	fi
done

# go
go get -u -v github.com/golang/lint/golint
