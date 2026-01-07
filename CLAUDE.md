# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

このリポジトリは178inaba氏の個人dotfilesコレクションです。GNU Stowを使用してシンボリックリンクベースで設定ファイルを管理しています。

## プロジェクト構造

```
├── claude/     # Claude AI設定
├── docs/       # ドキュメント・チートシート
├── ghostty/    # Ghosttyターミナル設定
├── git/        # Git設定（.gitconfig）
├── tmux/       # tmux設定（.tmux.conf）
├── vim/        # Vim設定（.vimrc）
└── zsh/        # Zsh設定（.zprofile, .zshrc）
```

各ディレクトリにはホームディレクトリに配置される設定ファイルが含まれています。

## セットアップコマンド

### 初期セットアップ
```zsh
git clone git@github.com:178inaba/dotfiles.git ~/.dotfiles
cd ~/.dotfiles
eval "$(/opt/homebrew/bin/brew shellenv)"
brew install git vim go ccat diff-so-fancy direnv nodenv stow tmux
stow git vim zsh claude tmux ghostty
zsh -l
```

### Stow操作
- 設定を適用: `stow <ディレクトリ名>`
- 設定を削除: `stow -D <ディレクトリ名>`
- 利用可能なパッケージ: git, vim, zsh, claude, tmux, ghostty

## 設定の特徴

- **Git**: diff-so-fancy、SSH URL書き換え、Go code review用エイリアス
- **Vim**: vim-plug、GitHub Copilot、Go/Terraform/Vue.js開発環境、Solarized8テーマ
- **Zsh**: Git統合プロンプト、豊富なエイリアス、direnv/gcloud統合
- **tmux**: 初心者向け設定、Vimスタイル操作、Solarized配色、詳細チートシート
- **Ghostty**: Solarized Dark Higher Contrastテーマ
- **Claude**: 日本語対応設定、カスタムコマンド、構造化されたコンテキスト

## ローカル設定

以下のローカル設定ファイルがサポートされています：
- `~/.gitconfig.local`
- `~/.zshrc.local`
- `~/.zprofile.local`

## よく使用するエイリアス

### Git
- `gs`: ブランチ、ステータス、ユーザー情報表示
- `ga`: 全ファイルをステージング
- `gd`: diff表示
- `gdc`: cached diff表示

### その他
- `dfs`: dotfilesディレクトリへ移動
- `cat`: ccat（カラー表示）使用
- `tmux-help`: tmuxチートシートを色付きページングで表示

## Claude設定の詳細

### カスタムコマンド
- `/git-commit` - 変更を適切な単位でコミット
- `/git-push` - コミット済み変更をプッシュ（ブランチ指定可能）
- `/git-pr` - プルリクエスト作成
- `/git-ship` - 状況に応じた統合処理（コミット・プッシュ・PR作成）
- `/issue-handle` - Issueの調査から実装完了まで対応（デフォルト: Planモードで対話型、`--auto`で一気に実装、`--file`でファイルから仕様読み込み、`--english`で英語コミット）
- `/extract-context` - 会話からコンテキストを抽出してCLAUDE.mdに追記
- `/review-response` - GitHubレビューコメントの自動対応
- `/code-review` - コード差分を詳細にレビュー（ベースブランチ指定、Issue連携、未コミット差分対応）
- `/bestpractice` - プロジェクト慣習を無視した一般的なベストプラクティスを確認

### 設定ファイル構造
```
~/.claude/
├── CLAUDE.md           # グローバル基本方針
├── commands/           # スラッシュコマンド定義
├── context/            # 詳細コンテキスト
│   ├── patterns/       # 設計パターン集
│   ├── tools/          # ツール使用方法
│   └── workflows/      # 開発ワークフロー
├── hooks/              # イベントフック（通知等）
├── settings.json       # Claude Code設定
└── statusline.sh       # ステータスライン表示スクリプト
```

## 開発ワークフロー

### Git操作の段階別コマンド
1. **コミットのみ**: `/git-commit`
2. **プッシュ**: `/git-push [branch-name]`
3. **PR作成**: `/git-pr`
4. **統合処理**: `/git-ship`

### コンテキスト管理
- 汎用的なパターンは`~/.claude/CLAUDE.md`または`~/.claude/context/`に自動追記
- プロジェクト固有の内容はプロジェクトのCLAUDE.mdに追記

## テーマ統一
- **ターミナル**: Ghostty (Solarized Dark Higher Contrast)
- **Vim**: Solarized8 Dark
- **tmux**: Solarized配色のステータスバー
- **統一方針**: 全エディタ・ターミナルツールでSolarizedテーマ使用

## ドキュメント

### チートシート
- **tmux**: `docs/tmux-cheatsheet.md` - 初心者向け包括的リファレンス
- **参照方法**: `tmux-help` コマンドで色付きページング表示

## 重要な注意事項

- **コミット規約**: 小さな変更は一括、大きな変更は論理的に分割
- **ファイル形式**: POSIX標準準拠（ファイル末尾に改行必須）
- **Stow使用時**: シンボリックリンクの管理に注意
