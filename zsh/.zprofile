export PATH=~/bin:$PATH

# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Go
export PATH=$(go env GOPATH)/bin:$PATH

# Volta
export VOLTA_HOME="$HOME/.volta"
export PATH="$VOLTA_HOME/bin:$PATH"

# Load local
if [ -f ~/.zprofile.local ]; then
  . ~/.zprofile.local
fi
