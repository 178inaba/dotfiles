# dotfiles path
DF=~/.dotfiles

# git
. $DF/git/git-completion.bash
. $DF/git/git-prompt.sh

# go
export GOPATH=~/work/go
export PATH=~/work/go/bin:$PATH

# alias
alias em='emacs'
alias ll='ls -l'
alias la='ls -la'
alias h='history | grep'
alias c='clear; clear;'

# git alias
alias ga='git add -A'
alias gb='git checkout -b'
alias gc='git checkout'
alias gd='git diff --color | less -R'
alias gs='git branch -a; git status;'
alias gca='git commit -a -m'
alias gcm='git commit -m'
alias gp='git push -u origin'
alias gpm='git push -u origin master'

# load local
. ~/.bashrc.local
