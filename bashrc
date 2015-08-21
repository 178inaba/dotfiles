# dotfiles path
DF=~/.dotfiles

# git
. $DF/download/shellscript/git-completion.bash
. $DF/download/shellscript/git-prompt.sh

# go
export GOPATH=~/work/go
export PATH=~/work/go/bin:$PATH

# download bin path
export PATH=~/.dotfiles/download/bin:$PATH

# bash prompt
PROMPT_COMMAND='__git_ps1 "\u@\h:\w" "\\\$ "'
GIT_PS1_SHOWDIRTYSTATE=true
GIT_PS1_SHOWSTASHSTATE=true
GIT_PS1_SHOWUNTRACKEDFILES=true
GIT_PS1_SHOWUPSTREAM=auto
GIT_PS1_SHOWCOLORHINTS=true
GIT_PS1_HIDE_IF_PWD_IGNORED=true

# option for each OS
case $OSTYPE in
	# mac
	darwin*)
		LSCLR='-G'
		. /usr/local/Library/Contributions/brew_bash_completion.sh
		;;
	# linux
	linux*) LSCLR='--color' ;;
esac

# alias
alias ll="ls -l $LSCLR"
alias lh="ls -lh $LSCLR"
alias la="ls -la $LSCLR"
alias lah="ls -lah $LSCLR"
alias em='emacs'
alias h='history | grep'
alias c='clear; clear;'
alias dfiles="cd $DF"

# git alias
alias ga='git add -A'
alias gb='git checkout -b'
alias gc='git checkout'
alias gd='git d'
alias gs='git branch; git status; gu;'
alias gca='git commit -a -m'
alias gcm='git commit -m'
alias gp='git push -u origin'
alias gpm='git push -u origin master'
alias inaba='git config user.name "178inaba"; git config user.email "178inaba@users.noreply.github.com";'

# git funcs
gu() {
	if [ "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" ]; then
		echo "User: $(git config user.name) <$(git config user.email)>"
	fi
}

# load local
. ~/.bashrc.local
