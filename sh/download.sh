#!/bin/bash

set -eu

mkdir -pv $DF/dl/{bin,emacs,sh/zsh/completion}
curl -fsSL -o $DF/dl/sh/git-completion.bash https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash
curl -fsSL -o $DF/dl/sh/zsh/completion/_git https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.zsh
curl -fsSL -o $DF/dl/sh/git-prompt.sh https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh
curl -fsSL -o $DF/dl/emacs/sgml-mode-patch.el https://gist.githubusercontent.com/178inaba/b143a6f4621e0fcb240cf27e07312eef/raw/db49ed9caaafcb08f16a0389787a1991cbd70727/sgml-mode-patch.el
curl -fsSL -o $DF/dl/emacs/php-cs-fixer.el https://raw.githubusercontent.com/OVYA/php-cs-fixer/master/php-cs-fixer.el

# Use download files.
emacs --batch -f batch-byte-compile $DF/dl/emacs/*.el
