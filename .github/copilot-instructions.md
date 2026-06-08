# Copilot Instructions

このリポジトリは GNU Stow で管理する dotfiles 集です。トップレベルの各ディレクトリは `$HOME` に展開される設定ファイルのソースなので、作業時は常にリポジトリ内の元ファイルを編集します。

## コマンド

```zsh
# 初期セットアップ
eval "$(/opt/homebrew/bin/brew shellenv)"
brew install git vim go ccat diff-so-fancy direnv nodenv stow tmux
stow git vim zsh claude tmux ghostty
zsh -l

# 変更したパッケージを適用し直す
stow <package>

# パッケージの適用を外す
stow -D <package>
```

利用する Stow パッケージは `git`、`vim`、`zsh`、`claude`、`tmux`、`ghostty` です。

このリポジトリにはアプリケーションのような build / test / lint の共通コマンドはありません。単体テスト実行コマンドも定義されておらず、通常は対象パッケージを `stow` し直し、必要に応じてシェルや各ツールをリロードして確認します。

## 高レベルアーキテクチャ

- リポジトリ全体は Stow パッケージとして構成されており、`git/.gitconfig`、`tmux/.tmux.conf`、`vim/.vimrc`、`zsh/.zprofile`、`zsh/.zshrc`、`claude/.claude/*`、`ghostty/.config/ghostty/*` のように、配置先のホームディレクトリ構造をそのままリポジトリ内で再現しています。
- Zsh 設定は役割分担されています。`zsh/.zprofile` は PATH や Homebrew / Go / Volta の初期化、`zsh/.zshrc` は補完、プロンプト、エイリアス、`direnv`、`gcloud` 連携など対話操作向けの設定を担当します。
- AI アシスタント関連の中心は `claude/.claude/` です。ここに Claude 向けの基本方針、再利用する context 文書、hooks、skills、settings が集約されています。
- テーマは個別ではなく横断的に揃えています。Ghostty は Solarized Dark Higher Contrast、Vim は Solarized8、tmux は Solarized 系のステータスバー設定を使っており、端末・エディタ間で見た目を統一しています。

## 重要な慣習

- ユーザーとのやりとりは原則として日本語で行います。コミットメッセージ、コード、設定キー、外部仕様の引用は必要に応じて英語を使って構いません。
- `~/.gitconfig` や `~/.claude/CLAUDE.md` のようなホーム側の実体ではなく、必ず `git/.gitconfig`、`claude/.claude/CLAUDE.md` などリポジトリ内のソースを編集します。
- ローカル固有設定はリポジトリに直接埋め込まず、既存の上書きポイントを使います。`~/.gitconfig.local`、`~/.zshrc.local`、`~/.zprofile.local` が読み込まれ、Vim では現在ディレクトリ階層から `.vimrc.local` を探索して追加設定を読み込みます。
- `.editorconfig` に従い、UTF-8、LF、Markdown 以外では末尾空白を削除し、必ず最終行に改行を入れます。
- ドキュメントやアシスタント向け設定は日本語が主ですが、既存ファイル内で英語のコメントや識別子を使っている箇所では、そのファイルの文体と慣習を優先します。
- コメントは最小限にし、自明な説明は避けます。設計判断の理由、既知の制約、非自明な回避策の説明が必要な場合にだけ追加します。
