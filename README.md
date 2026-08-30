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
$ brew install tmux git vim go ccat diff-so-fancy direnv nodenv stow gh jq yq
$ stow tmux git vim zsh claude ghostty shims
$ zsh -l
$ gh auth login
```

`git` must be 2.37 or newer, which the `brew install` above satisfies:
`git/.gitconfig` sets `branch.autoSetupMerge = simple`, and older git dies with
`bad boolean config value` on every command.

`gh`, `jq` and `yq` are required by the Claude Code scripts (hooks, skills, statusline).
Run `gh auth login` to authenticate the GitHub CLI after installing.

### Optional CLI tools

Some Claude Code skills wrap a dedicated CLI. They are optional — without them
the corresponding skill just reports the missing command and stops.

```zsh
$ go install github.com/178inaba/cflio@latest  # Confluence (cflio skill)
$ go install github.com/178inaba/rdsh@latest   # Redash (rdsh skill)
$ go install github.com/178inaba/slio@latest   # Slack (slio skill)
```

Each requires a one-time `<command> auth login`.

The skills themselves are not copied into this repo. Every one of these CLIs
ships its own Claude Code plugin, declared in `claude/.claude/settings.json`, so
Claude Code registers the marketplace, installs the plugin and keeps it current
by itself — no step beyond the usual `git pull` and `stow -R`. Registration and
installation run in the background after a session starts, so on a new machine
the skills appear over the first couple of sessions rather than instantly. They
are namespaced by their plugin: `/cflio:cflio`, `/rdsh:rdsh`, `/slio:slio`.

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

## Packages

- `claude`: Claude Code configuration
- `ghostty`: Ghostty terminal configuration
- `git`: Git configuration
- `shims`: PATH shims that wrap installed commands (currently `gh`)
- `tmux`: tmux configuration
- `vim`: Vim configuration
- `zsh`: Zsh configuration

## License

[MIT](LICENSE)

## Author

Masahiro Furudate (a.k.a. [178inaba](https://github.com/178inaba))  
<178inaba.git@gmail.com>
