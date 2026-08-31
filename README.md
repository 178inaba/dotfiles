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
$ brew install tmux git vim go ccat diff-so-fancy direnv nodenv stow gh jq yq 178inaba/tap/cflio 178inaba/tap/rdsh 178inaba/tap/slio
$ stow tmux git vim zsh claude ghostty shims
$ zsh -l
$ go -C go install ./cmd/ccx
$ gh auth login
$ cflio auth login
$ rdsh auth login
$ slio auth login
```

`git` must be 2.37 or newer, which the `brew install` above satisfies:
`git/.gitconfig` sets `branch.autoSetupMerge = simple`, and older git dies with
`bad boolean config value` on every command.

`gh`, `jq` and `yq` are required by the Claude Code scripts (shared and skill-bundled).

`go -C go install ./cmd/ccx` is the only build step, and only the first one:
`ccx` compares its own timestamp with the newest file under `go/` on every
start and reinstalls itself when it is behind.

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
$ stow -R tmux git vim zsh claude ghostty shims
```

Always restow. Some pulls need it and some do not, and telling the two apart is
a judgement that fails silently when it goes wrong — the new files simply never
appear. `stow -R` is idempotent and takes milliseconds, so there is no reason to
make that call.

`ccx` is not on the `stow` line and does not need to be rebuilt by hand: the
first invocation after a pull notices that the source is newer than the binary
and reinstalls itself.

## Packages

- `claude`: Claude Code configuration
- `ghostty`: Ghostty terminal configuration
- `git`: Git configuration
- `shims`: PATH shims that wrap installed commands (currently `gh`)
- `tmux`: tmux configuration
- `vim`: Vim configuration
- `zsh`: Zsh configuration

`go/` is not a stow package. It is the Go module the Claude Code tooling is
being moved into, built with `go -C go install ./cmd/ccx`; the binaries it
produces are never committed.

## License

[MIT](LICENSE)

## Author

Masahiro Furudate (a.k.a. [178inaba](https://github.com/178inaba))  
<178inaba.git@gmail.com>
