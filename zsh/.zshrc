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
setopt PROMPT_SUBST

## Command execution time tracking
preexec() {
  timer=${timer:-$SECONDS}
}

precmd() {
  exec_time=''
  if [[ -n $timer ]]; then
    local elapsed=$(($SECONDS - $timer))
    if (( elapsed >= 5 )); then
      exec_time="[${elapsed}s]"
    fi
    unset timer
  fi
}

PS1='[%* %~$(__git_ps1 " (%s)")]\$ '
RPROMPT='${exec_time}'
# Remove RPROMPT from accepted lines so copied scrollback stays clean
setopt TRANSIENT_RPROMPT

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

# Tmux cheatsheet
tmux-help() {
  local sheet="${HOME}/.dotfiles/docs/tmux-cheatsheet.md"
  if ! command -v ccat >/dev/null; then
    echo 'tmux-help: ccat not found' >&2
    return 1
  fi
  if [[ ! -r "${sheet}" ]]; then
    echo "tmux-help: cannot read ${sheet}" >&2
    return 1
  fi
  # Plain text off a terminal: a pipe or redirect should get neither escapes nor a pager.
  if [[ -t 1 ]]; then
    ccat --color=always --bg=dark "${sheet}" | less -R
  else
    ccat --color=never "${sheet}"
  fi
}

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
eval "$(mise activate zsh)"

# The next line updates PATH for the Google Cloud SDK.
if [ -f "${HOME}/google-cloud-sdk/path.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/path.zsh.inc"; fi

# The next line enables shell command completion for gcloud.
if [ -f "${HOME}/google-cloud-sdk/completion.zsh.inc" ]; then . "${HOME}/google-cloud-sdk/completion.zsh.inc"; fi

# Load local
if [ -f "${HOME}/.zshrc.local" ]; then
  . "${HOME}/.zshrc.local"
fi
