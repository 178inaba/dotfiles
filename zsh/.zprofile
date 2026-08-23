export PATH="${HOME}/.local/bin:${HOME}/bin:${PATH}"

# Homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"

# Go
export PATH="$(go env GOPATH)/bin:${PATH}"

# Shims (after Homebrew and Go so a shim shadows the binary they provide)
export PATH="${HOME}/.local/shims:${PATH}"

# Load local
if [ -f "${HOME}/.zprofile.local" ]; then
  . "${HOME}/.zprofile.local"
fi
