# only mac
if [ $(echo $OSTYPE | grep darwin) ]; then
	# bash-completion load
	BASH_COMP_DIR=$(brew --prefix)
	if [ -f $BASH_COMP_DIR/etc/bash_completion ]; then
		. $BASH_COMP_DIR/etc/bash_completion
	fi
fi

# path
export PATH="/usr/local/sbin:$PATH"

# go
export GOPATH=~/work/go
export PATH=~/work/go/bin:$PATH

# composer(php)
export PATH=~/.composer/vendor/bin:$PATH

# download bin path
export PATH=~/.dotfiles/dl/bin:$PATH

# load local
if [ -f ~/.bash_profile.local ]; then
    . ~/.bash_profile.local
fi

# load .bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
