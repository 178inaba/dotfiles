#!/bin/sh

# dotfiles path
DF=~/.dotfiles

# git
wget https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -O $DF/git-completion.bash
wget https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh -O $DF/git-prompt.sh
