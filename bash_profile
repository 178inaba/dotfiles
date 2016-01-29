# only mac
if [ $(echo $OSTYPE | grep darwin) ]; then
	# bash-completion load
	BASH_COMP_DIR=$(brew --prefix)
	if [ -f $BASH_COMP_DIR/etc/bash_completion ]; then
		. $BASH_COMP_DIR/etc/bash_completion
	fi

	eval "$(docker-machine env default)"
fi

# path
export PATH="/usr/local/sbin:$PATH"

# go
export GOPATH=~/work/go
export PATH=~/work/go/bin:$PATH

# composer(php)
export PATH=~/.composer/vendor/bin:$PATH

# download bin path
export PATH=~/.dotfiles/download/bin:$PATH

# load local
. ~/.bash_profile.local
