tap "178inaba/tap"

brew "tmux"
brew "git"
brew "vim"
brew "go"
brew "ccat"
brew "diff-so-fancy"
brew "direnv"
brew "nodenv"
brew "mise"
brew "stow"
# gh is not called for the GitHub API directly (go-gh runs in process), but go-gh
# reads the OAuth token `gh auth login` stores, and the shim in ~/.local/shims
# hands the real gh everything it does not refuse.
brew "gh"
# jq is what the skills read ccx's JSON output with.
brew "jq"
brew "178inaba/tap/cflio"
brew "178inaba/tap/rdsh"
brew "178inaba/tap/slio"

# lsof ships with macOS and isn't listed above: `ccx worktree collect` and
# `ccx worktree delete` use it to find the processes sitting in a worktree, so
# that removing one doesn't kill a running session.
