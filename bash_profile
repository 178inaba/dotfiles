# get dir for mac
if [ $(echo $OSTYPE | grep darwin) ]; then
	BASH_COMP_DIR=$(brew --prefix)
fi

# bash-completion load
if [ -f $BASH_COMP_DIR/etc/bash_completion ]; then
	. $BASH_COMP_DIR/etc/bash_completion
fi

# path
export PATH="/usr/local/sbin:$PATH"

# load local
. ~/.bash_profile.local
