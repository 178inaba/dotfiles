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
$ brew install tmux git vim go ccat diff-so-fancy direnv nodenv stow
$ stow tmux git vim zsh claude copilot ghostty opencode
$ zsh -l
```

## Packages

- `claude`: Claude Code configuration
- `copilot`: GitHub Copilot CLI configuration
- `ghostty`: Ghostty terminal configuration
- `git`: Git configuration
- `opencode`: OpenCode configuration
- `tmux`: tmux configuration
- `vim`: Vim configuration
- `zsh`: Zsh configuration

## License

[MIT](LICENSE)

## Author

Masahiro Furudate (a.k.a. [178inaba](https://github.com/178inaba))  
<178inaba.git@gmail.com>
