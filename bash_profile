# only mac
if [ $(echo $OSTYPE | grep darwin) ]; then
    # bash-completion load
    BASH_COMP_DIR=$(brew --prefix)
    if [ -f $BASH_COMP_DIR/etc/bash_completion ]; then
        . $BASH_COMP_DIR/etc/bash_completion
    fi

    # Android tools
    export PATH=~/Library/Android/sdk/platform-tools:$PATH

    # Java (Android Studio)
    export PATH='/Applications/Android Studio.app/Contents/jre/jdk/Contents/Home/bin':$PATH
fi

# path
export PATH=~/bin:/usr/local/sbin:$PATH

# go
export PATH=/usr/local/go/bin:~/go/bin:$PATH

# composer(php)
export PATH=~/.composer/vendor/bin:$PATH

# flutter
export PATH=~/tools/flutter/bin:$PATH

# node
eval "$(nodenv init -)"

# download bin path
export PATH=~/.dotfiles/dl/bin:$PATH

# grep option
export GREP_OPTIONS='--color=auto'

# load local
if [ -f ~/.bash_profile.local ]; then
    . ~/.bash_profile.local
fi

# load .bashrc
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
