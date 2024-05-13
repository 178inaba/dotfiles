# only mac
if [ $(echo $OSTYPE | grep darwin) ]; then
  # Android tools
  export PATH=~/Library/Android/sdk/platform-tools:$PATH

  # Homebrew
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi

setopt interactivecomments

# path
export PATH=~/bin:/usr/local/sbin:$PATH

# go
export PATH=/usr/local/go/bin:~/go/bin:$PATH

# composer(php)
export PATH=~/.composer/vendor/bin:$PATH

# flutter
export PATH=~/tools/flutter/bin:$PATH

# volta
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

# grep option
export GREP_OPTIONS='--color=auto'

# load local
if [ -f ~/.zprofile.local ]; then
  . ~/.zprofile.local
fi
