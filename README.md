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
$ brew install tmux git vim go ccat diff-so-fancy direnv nodenv stow gh jq
$ stow tmux git vim zsh claude ghostty
$ zsh -l
$ gh auth login
```

`gh` and `jq` are required by the Claude Code scripts (hooks, skills, statusline).
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

## Packages

- `claude`: Claude Code configuration
- `ghostty`: Ghostty terminal configuration
- `git`: Git configuration
- `tmux`: tmux configuration
- `vim`: Vim configuration
- `zsh`: Zsh configuration

## License

[MIT](LICENSE)

## Author

Masahiro Furudate (a.k.a. [178inaba](https://github.com/178inaba))  
<178inaba.git@gmail.com>
