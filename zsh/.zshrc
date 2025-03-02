setopt interactivecomments

# Completion
autoload -Uz compinit
compinit

# Prompt
. $(brew --prefix git)/etc/bash_completion.d/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM=auto
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_HIDE_IF_PWD_IGNORED=1
setopt PROMPT_SUBST ; PS1='[%n@%m %c$(__git_ps1 " (%s)")]\$ '

# Alias
alias ls='ls --color=auto'
alias la='ls -la'
alias h='history 1 | grep --color=auto'
alias dfs='cd ~/.dotfiles'
alias cat='ccat --bg=dark'

# Git alias
alias ga='git add -A'
alias gd='git diff'
alias gp='git push -u origin'
alias gs='git branch && git status && gu'
alias gca='git commit -a -m'
alias gcm='git commit -m'
alias gfp='git fetch --prune'
alias gpp='git pull --prune'
alias gsu='git submodule update'

# Recursive search under the current directory
gre() {
  grep -nr --color=auto "$@" .
}

# Git user
gu() {
  if [ "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" ]; then
    echo "User: $(git config user.name) <$(git config user.email)>"
  fi
}

# Hooks
eval "$(direnv hook zsh)"
eval "$(nodenv init -)"

# Load local
if [ -f ~/.zshrc.local ]; then
  . ~/.zshrc.local
fi
