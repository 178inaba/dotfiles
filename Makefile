# Homebrew isn't on PATH yet on a fresh machine (zsh/.zprofile applies only after
# `stow`), so add it here for `make install` itself.
export PATH := /opt/homebrew/bin:$(PATH)

PACKAGES := tmux git vim zsh claude ghostty herdr

.PHONY: install bundle stow mise build

# Every step is idempotent, so the same target serves a fresh machine and a pull.
install: bundle
	$(MAKE) -j stow mise build

bundle:
	brew bundle install

# herdr keeps its sockets, logs and session state next to config.toml, so
# ~/.config/herdr has to be a real directory: stow would otherwise fold it into
# a symlink and herdr would write all of that into this repository.
stow:
	mkdir -p ~/.config/herdr
	stow -R $(PACKAGES)

mise:
	mise -C go install

# GOBIN, not ~/go/bin, for the `gh` shim below: that's a shared namespace where a
# stray `go install github.com/cli/cli/v2/cmd/gh@latest` would overwrite the shim.
build:
	go -C go install ./cmd/ccx
	mkdir -p ~/.local/shims
	GOBIN=~/.local/shims go -C go install ./cmd/gh
