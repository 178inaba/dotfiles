setopt interactivecomments

# Completion
FPATH="${HOME}/.docker/completions:${FPATH}"
autoload -Uz compinit
compinit -C

# Terraform completion
autoload -U +X bashcompinit && bashcompinit
complete -o nospace -C $(brew --prefix terraform)/bin/terraform terraform

# Prompt
. $(brew --prefix git)/etc/bash_completion.d/git-prompt.sh
GIT_PS1_SHOWDIRTYSTATE=1
GIT_PS1_SHOWSTASHSTATE=1
GIT_PS1_SHOWUNTRACKEDFILES=1
GIT_PS1_SHOWUPSTREAM=auto
GIT_PS1_SHOWCOLORHINTS=1
GIT_PS1_HIDE_IF_PWD_IGNORED=1
setopt PROMPT_SUBST ; PS1='[%* %~$(__git_ps1 " (%s)")]\$ '

# Alias
alias ls='ls --color=auto'
alias la='ls -la'
alias h='history 1 | grep --color=auto'
alias dfs="cd ${HOME}/.dotfiles"
alias cat='ccat --bg=dark'

# Git alias
alias ga='git add -A'
alias gd='git diff'
alias gdc='git diff --cached'
alias gp='git push -u origin'
alias gs='git branch && git status && gu'
alias gca='git commit -a -m'
alias gcm='git commit -m'
alias gfp='git fetch --prune'
alias gpp='git pull --prune'
alias gsu='git submodule update'

# Tmux alias
alias tmux-help='ccat --color=always --bg=dark ~/.dotfiles/docs/tmux-cheatsheet.md | less -R'

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

# The next line updates PATH for the Google Cloud SDK.
if [ -f "${HOME}/google-cloud-sdk/path.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "${HOME}/google-cloud-sdk/completion.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/completion.zsh.inc"; fi

# Load local
if [ -f "${HOME}/.zshrc.local" ]; then
  . "${HOME}/.zshrc.local"
fi
