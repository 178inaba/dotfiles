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
alias h='history 1 | grep'
alias dfiles='cd ~/.dotfiles'
alias cat='ccat --bg=dark'
alias goc='go test -coverprofile=/tmp/goc && go tool cover -html=/tmp/goc && rm /tmp/goc'

# git alias
alias ga='git add -A'
alias gb='git checkout -b'
alias gc='git checkout'
alias gd='git diff'
alias gs='git branch && git status && gu'
alias gca='git commit -a -m'
alias gcm='git commit -m'
alias gp='git push -u origin'
alias gpm='git push -u origin master'
alias gpd='git push -u origin develop'
alias gpp='git pull --prune'

# funcs
docker() {
  if [[ $@ == 'reset' ]]; then
    # clean
    docker stop $(docker ps -aq)
    docker rm $(docker ps -aq)
    docker rmi $(docker images -q)

    # check
    docker ps -a
    docker images
  else
    command docker "$@"
  fi
}

# Grep as the current directory.
gre() {
  grep -nr --color=auto "$@" .
}

# git funcs
gu() {
  if [ "true" = "$(git rev-parse --is-inside-work-tree 2>/dev/null)" ]; then
    echo "User: $(git config user.name) <$(git config user.email)>"
  fi
}

# Hooks
eval "$(direnv hook zsh)"
eval "$(nodenv init -)"

# load local
if [ -f ~/.zshrc.local ]; then
  . ~/.zshrc.local
fi
