# Homebrew isn't on PATH yet on a fresh machine (zsh/.zprofile applies only after
# `stow`), so add it here for `make setup` itself.
export PATH := /opt/homebrew/bin:$(PATH)

PACKAGES := tmux git vim zsh claude ghostty

.PHONY: setup update bundle stow mise build

setup: bundle
	$(MAKE) -j stow mise build

# --no-upgrade: a routine update shouldn't also upgrade every outdated formula.
bundle:
	brew bundle install --no-upgrade

update: bundle
	$(MAKE) -j stow mise

# stow -R is idempotent, so update always runs it -- no need to judge whether a
# pull actually added new dotfiles state.
stow:
	stow -R $(PACKAGES)

mise:
	mise -C go install

# GOBIN, not ~/go/bin, for the `gh` shim below: that's a shared namespace where a
# stray `go install github.com/cli/cli/v2/cmd/gh@latest` would overwrite the shim.
build:
	go -C go install ./cmd/ccx
	mkdir -p ~/.local/shims
	GOBIN=~/.local/shims go -C go install ./cmd/gh
