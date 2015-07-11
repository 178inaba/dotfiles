# git
. ~/dotfiles/git-completion.bash
. ~/dotfiles/git-prompt.sh

# go
export GOPATH=~/work/go
export PATH=~/work/go/bin:$PATH

# alias
alias em='emacs'
alias ll='ls -l'
alias h='history | grep'
alias gs='git branch; git status;'
alias gd='git diff --color | less -R'
alias c='clear;clear;'

# load local
. ~/.bashrc.local
