#!/bin/sh

# git
mkdir -v $DF/git
curl -fsSL -o $DF/git/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -fsSL -o $DF/git/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
