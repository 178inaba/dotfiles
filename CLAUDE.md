# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

このリポジトリは178inaba氏の個人dotfilesコレクションです。GNU Stowを使用してシンボリックリンクベースで設定ファイルを管理しています。

## プロジェクト構造

```
├── claude/     # Claude AI設定
├── git/        # Git設定（.gitconfig）
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
brew install git vim go ccat diff-so-fancy direnv nodenv stow
stow git vim zsh claude
zsh -l
```

### Stow操作
- 設定を適用: `stow <ディレクトリ名>`
- 設定を削除: `stow -D <ディレクトリ名>`
- 利用可能なパッケージ: git, vim, zsh, claude

## 設定の特徴

- **Git**: diff-so-fancy、SSH URL書き換え、Go code review用エイリアス
- **Vim**: vim-plug、GitHub Copilot、Go/Terraform/Vue.js開発環境、Solarized8テーマ
- **Zsh**: Git統合プロンプト、豊富なエイリアス、direnv/gcloud統合
- **Claude**: 日本語対応設定、カスタムコマンド

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

## Claude設定の詳細

### カスタムコマンド
- `/git-commit` - 変更をコミットのみ
- `/git-push` - コミット + プッシュ（ブランチ指定可能）
- `/git-pr` - プルリクエスト作成
- `/git-ship` - 完全なワークフロー実行

### 設定ファイル
- `~/.claude/CLAUDE.md` - グローバル設定
- `~/.claude/context/` - 詳細コンテキストファイル

## 開発ワークフロー

### Git操作の段階別コマンド
1. **コミットのみ**: `/git-commit`
2. **コミット + プッシュ**: `/git-push [branch-name]`
3. **プルリクエスト作成**: `/git-pr`

## テーマ統一
- **ターミナル**: Ghostty (Solarized Dark Higher Contrast)
- **Vim**: Solarized8 Dark
- **統一方針**: 全エディタでSolarizedテーマ使用
