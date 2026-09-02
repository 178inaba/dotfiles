export PATH="${HOME}/.local/bin:${HOME}/bin:${PATH}"

# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Go
export PATH="$(go env GOPATH)/bin:${PATH}"

# Shims (after Homebrew and Go so a shim shadows the binary they provide)
export PATH="${HOME}/.local/shims:${PATH}"

# mise activate in .zshrc only rewrites PATH from precmd/chpwd hooks, and a
# non-interactive shell registers neither, so this directory is what makes
# `cd go && golangci-lint run` resolve the version pinned there too
export PATH="${HOME}/.local/share/mise/shims:${PATH}"

# Load local
if [ -f "${HOME}/.zprofile.local" ]; then
  . "${HOME}/.zprofile.local"
fi
