# dotfiles

This is my dotfiles.

## Requirements

- [Adding a new SSH key to your GitHub account](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account)
- [Homebrew](https://brew.sh/)

## Setup

```zsh
$ git clone git@github.com:178inaba/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ eval "$(/opt/homebrew/bin/brew shellenv)"
$ brew install tmux git vim go ccat diff-so-fancy direnv nodenv mise stow gh jq 178inaba/tap/cflio 178inaba/tap/rdsh 178inaba/tap/slio
$ stow tmux git vim zsh claude ghostty
$ zsh -l
$ mise -C go install
$ go -C go install ./cmd/ccx
$ mkdir -p ~/.local/shims
$ GOBIN=~/.local/shims go -C go install ./cmd/gh
$ gh auth login
$ cflio auth login
$ rdsh auth login
$ slio auth login
```

`git` must be 2.37 or newer, which the `brew install` above satisfies:
`git/.gitconfig` sets `branch.autoSetupMerge = simple`, and older git dies with
`bad boolean config value` on every command.

Three commands are required at run time. `gh` is not run for the GitHub API —
that goes through go-gh in process — but go-gh reads the OAuth token `gh auth
login` stores, and the shim in `~/.local/shims` hands the real one everything
it does not refuse. `jq` is what the skills read `ccx`'s JSON output with.
`lsof` is how `ccx worktree collect` and `ccx worktree delete` find the
processes sitting in a worktree, so that removing one does not kill a running
session; it ships with macOS and is not on the `brew install` line.

The two `go install` lines are the only build step, and only the first one:
each binary compares its own timestamp with the newest file under `go/` on
every start and reinstalls both when it is behind. `~/.local/shims` holds the
`gh` shim, which `zsh/.zprofile` puts ahead of Homebrew on PATH; the directory
is filled by `go install` rather than by `stow`, and it is not `~/go/bin`
because that is a shared namespace where `go install
github.com/cli/cli/v2/cmd/gh@latest` would overwrite the shim.

### Claude Code plugins

`cflio`, `rdsh` and `slio` — the Confluence, Redash and Slack CLIs installed
above — each back a Claude Code skill, and the skills themselves are not copied
into this repo. Each CLI ships its own plugin, declared in
`claude/.claude/settings.json`, so Claude Code registers the marketplace,
installs the plugin and keeps it current by itself — no step beyond the usual
`git pull` and `stow -R`. Registration and installation run in the background
after a session starts, so on a new machine the skills appear over the first
couple of sessions rather than instantly. They are namespaced by their plugin:
`/cflio:cflio`, `/rdsh:rdsh`, `/slio:slio`.

## Update

Applying changes on a machine that is already set up:

```zsh
$ cd ~/.dotfiles
$ git pull
$ stow -R tmux git vim zsh claude ghostty
```

Always restow. Some pulls need it and some do not, and telling the two apart is
a judgement that fails silently when it goes wrong — the new files simply never
appear. `stow -R` is idempotent and takes milliseconds, so there is no reason to
make that call.

`ccx` and the `gh` shim are not on the `stow` line and do not need to be
rebuilt by hand: the first invocation of either after a pull notices that the
source is newer than the binary and reinstalls both.

## Packages

- `claude`: Claude Code configuration
- `ghostty`: Ghostty terminal configuration
- `git`: Git configuration
- `tmux`: tmux configuration
- `vim`: Vim configuration
- `zsh`: Zsh configuration

`go/` is not a stow package. It is the Go module the Claude Code tooling lives
in — `ccx` for the status line, the hooks and the skill plumbing, and `gh` for
the shim — and the binaries it produces are never committed. `go/.tool-versions`
pins golangci-lint for it, read by both mise locally and the lint job in CI.

## License

[MIT](LICENSE)

## Author

Masahiro Furudate (a.k.a. [178inaba](https://github.com/178inaba))  
<178inaba.git@gmail.com>
