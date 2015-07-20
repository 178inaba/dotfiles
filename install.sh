#!/bin/sh

# install package
PKG=(go)

for I in ${PKG[@]}
do
	install $I
done
