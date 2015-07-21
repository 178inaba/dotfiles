# dotfiles path
DF=~/.dotfiles

# git
. $DF/git/git-completion.bash
. $DF/git/git-prompt.sh

# go
export GOPATH=~/work/go
export PATH=~/work/go/bin:$PATH

# bash prompt
export PS1='\h:\W \u\$ '

# option for each OS
case $OSTYPE in
	# mac
	darwin*) LSCLR='-G';;
	# linux
	linux*)  LSCLR='--color';;
esac

# alias
alias ll="ls -l $LSCLR"
alias la="ls -la $LSCLR"
alias em='emacs'
alias h='history | grep'
alias c='clear; clear;'

# git alias
alias ga='git add -A'
alias gb='git checkout -b'
alias gc='git checkout'
alias gd='git diff --color | less -R'
alias gs='git branch; git status; gu;'
alias gca='git commit -a -m'
alias gcm='git commit -m'
alias gp='git push -u origin'
alias gpm='git push -u origin master'

# git funcs
gu() {
	if [ -d .git ]; then
		echo "User: `git config user.name` <`git config user.email`>"
	fi
}

# load local
. ~/.bashrc.local
