#!/bin/sh

install() {
	if ! type $1 >/dev/null 2>&1; then
		$PKGMGR install $1
	fi
}
