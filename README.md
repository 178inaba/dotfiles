# dotfiles

This is my dotfiles.

## Requirements

- [Adding a new SSH key to your GitHub account](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account)
- [Homebrew](https://brew.sh/)

## Setup

```zsh
$ git clone git@github.com:178inaba/dotfiles.git ~/.dotfiles
$ cd ~/.dotfiles
$ make setup
$ zsh -l
$ gh auth login
$ cflio auth login
$ rdsh auth login
$ slio auth login
```

`make setup` installs the Brewfile packages, stows the packages listed in the
`Makefile`, fetches the golangci-lint version `go/.tool-versions` pins, and builds
`ccx` and the `gh` shim — see `Makefile` and `Brewfile` for what each step does and
why. `zsh -l` must come before the `auth login` commands: it is what puts the
just-installed `gh`, `cflio`, `rdsh` and `slio` on `PATH` (`zsh/.zprofile` only
takes effect in a shell started after `stow`).

### Claude Code plugins

`cflio`, `rdsh` and `slio` each back a Claude Code skill (`/cflio:cflio`, `/rdsh:rdsh`,
`/slio:slio`) supplied by their own plugin through `claude/.claude/settings.json`, not
copied into this repo. Claude Code registers and installs each plugin itself in the
background, so on a new machine the skills appear over the first couple of sessions
rather than instantly.

## Update

Applying changes on a machine that is already set up:

```zsh
$ cd ~/.dotfiles
$ git pull
$ make update
```

`ccx` and the `gh` shim aren't rebuilt by `make update`: the first invocation of
either after a pull notices the source is newer than the binary and reinstalls
both, so no manual step is needed for them either.

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
