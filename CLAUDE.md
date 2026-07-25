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
- **Claude**: 日本語対応設定、スキル、構造化されたコンテキスト

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

### スキル
- `/git-commit` - 変更を適切な単位でコミット
- `/git-pr` - プルリクエスト作成（未プッシュなら自動プッシュ、既存PRがあれば更新）
- `/git-rebase` - PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消
- `/understand-pr` - 現在のブランチのPRを理解し、目的・変更内容・現状を構造化して報告
- `/issue-draft` - 壁打ちで固めた内容を「迷わず実装できる」Issueとして構造化・作成、`--refine <番号>` で既存Issueを同じ品質基準へブラッシュアップして本文更新（情報不足は追加調査とユーザー確認で補完、粒度はリリース単位のatomicity基準で判定しgreenfieldでも自然な境界がある場合のみSub-Issues分割を提案（diff行数等のプロセス都合を分割基準にしない）、削っても残りが成立する低価値部分はスコープ縮小候補として提示（価値の取捨チェック）、提示前に合意網羅チェック・デッドスペックチェック（消費者の無い構成要素の排除）と会話コンテキストを持たないサブエージェント（Opus）による新規読者チェックで自己完結を検証、作成・更新前にドラフト承認）
- `/issue-handle` - Issueの調査から実装完了まで対応（Planモードで対話型、`--file`でファイルから仕様読み込み、`--worktree`で専用worktreeに隔離して並列開発可能、`--delegate-impl`で実装ループを Sonnet サブエージェントに委譲（判断と外向き操作は親に残る）、計画承認前に `/deep-plan-review` で計画検証（`--no-plan-review` で軽微な Issue 向けにスキップ可、完了時レビューは残る）、完了時に独立セッション（Fable）で `/deep-review` を実行）
- `/review-response` - GitHubレビューコメントの自動対応（`<pr-number>`でPR指定、`--worktree`で対象PRのworktreeに切替/作成、`--dry-run`で確認のみ）
- `/deep-review` - コード差分を詳細にレビュー（ベースブランチは自動判定、`--issue`でIssue連携、`<pr-number> --worktree`で対象PRのworktreeに切替/作成して並列レビュー）。Claude Code 2.1.146以降は組み込み `/code-review`・`/simplify` と区別するため `deep-review` 命名
- `/check-plan-compliance` - 現計画とプロジェクトCLAUDE.md・リンク先文書との準拠チェック後、計画修正と plan モード復帰まで自動実行
- `/deep-plan-review` - 計画の参照実在性・前提と実コードの一致・設計妥当性・自己完結性を、会話コンテキストを持たないサブエージェント（Opus）で検証し、blocker なしへの収束まで計画修正と plan モード復帰を自動実行（`/check-plan-compliance` 併用時は compliance 先）
- `/cleanup-merged` - マージ済みのworktreeとlocal branchをまとめてクリーンアップ（`--dry-run`で確認のみ、`--yes`で確認スキップ、`--include-closed`でCLOSED状態のPRも対象）
- `/review-assigned-prs` - 自分にレビュー依頼が来ているPRのうち Bot 以外のレビューが未着のものを `/deep-review <PR番号> --worktree --no-autofix` で並列レビュー（`/loop 5m /review-assigned-prs` で常駐運用推奨）
- `/bestpractice` - プロジェクト慣習を無視した一般的なベストプラクティスを確認
- `/troubleshooting` - エラー調査・デバッグの具体的手法（エラー調査タスク時に自動ロードされる知識スキル）
- `/test-implementation` - テストコードの品質評価3原則（テスト作成・レビュータスク時に自動ロードされる知識スキル。旧 `context/test-implementation.md` から移行）
- `/github-sub-issues` - GitHub Sub-Issuesの作成・リンク手順（Sub-Issue操作タスク時に自動ロードされる知識スキル）
- `worktree-resolution` - PR・ブランチに対応するworktreeの解決手順と命名規約（`--worktree` を持つスキルから参照される共有知識スキル。ユーザー直接起動は不可）
- `finding-triage` - レビュー指摘・修正提案（finding）を見送る前の検証規律と、繰り返される同種見送りの規約昇格。`deep-review`・`review-response`・`issue-handle` から参照される共有知識スキル（ユーザー直接起動は不可）

### 設定ファイル構造
```
~/.claude/
├── CLAUDE.md           # グローバル基本方針（常時ロード）
├── agents/             # カスタムサブエージェント定義（frontmatter で model を固定し、呼び出し側の指定漏れによる親モデル継承を構造的に防ぐ。independent-reviewer = issue-handle 完了時レビュー用 Fable 固定、fresh-reader = deep-plan-review / issue-draft の新規読者検証用 Opus 固定・読み取り専用）
├── skills/             # スキル定義（タスク起点で自動ロード）
├── rules/              # ルール（frontmatterのpaths globに該当するファイルを扱うときだけ遅延読込）
├── context/            # 詳細コンテキスト（CLAUDE.mdから@importで常時ロード）
├── scripts/            # スキル横断で共有するスクリプト（fetch-pr-context.sh等）
├── hooks/              # イベントフック（通知・事故防止等）
│   └── tests/          # フックのリグレッションテスト
├── tests/              # ルート直下スクリプト（statusline.sh 等）のリグレッションテスト（テスト配置規約の正は rules/script-testing.md）
├── settings.json       # Claude Code設定
└── statusline.sh       # ステータスライン表示スクリプト
```

コンテキストの置き場所は「トリガーの性質」で振り分ける: 常時必要な原則 → CLAUDE.md、ファイル起点 → rules/、タスク起点 → skills/（詳細はグローバルCLAUDE.mdの「コンテキスト管理」参照）

### Hooks

各フックの設計意図（何を防ぐために存在するか）は `claude/.claude/rules/hooks-design.md` を参照（フック・`settings.json` 編集時に自動ロードされる）。

### スキルスクリプト
- スキル内の決定的処理（収集・判定・正規化）はスクリプトに分離し、判断が必要な処理だけを SKILL.md の指示として残す（規約: `claude/.claude/rules/skill-authoring.md` の「スクリプト同梱パターン」）
  - `skills/cleanup-merged/scripts/collect-candidates.sh` — 削除候補の収集・マージ判定・セーフティチェック
  - `skills/review-assigned-prs/scripts/list-pending-reviews.sh` — Bot 以外のレビュー未着 PR の候補収集・判定
  - `skills/review-assigned-prs/scripts/ensure-clone.sh` — レビュー用 clone dir の ensure（未 clone は clone、既存は fetch）
  - `skills/review-assigned-prs/scripts/verify-posted-reviews.sh` — サブエージェント完了報告後のレビュー投稿検証
  - `skills/worktree-resolution/scripts/resolve-pr-worktree.sh` — PR worktree 解決の配管（`resolve` / `create-fallback` / `finalize` の3サブコマンド。作成・切替プリミティブの分岐だけを AI に残す）
  - `skills/worktree-resolution/scripts/check-pr-freshness.sh` — PR head との鮮度確認と安全な ff 自動同期（`/deep-review`・`/review-response` 共有）
  - `skills/deep-review/scripts/prepare-review.sh` — レビュー準備の集約（フラグ検証・PR 存在プローブ・コンテキスト取得・3モード判定・鮮度確認・ベースブランチ判定）
  - `skills/deep-review/scripts/post-review.sh` — レビュー投稿（総合評価→event 決定表・投稿前の行番号検証・REST 投稿）
  - `scripts/fetch-pr-context.sh` — PR コンテキスト一括取得（`/deep-review`・`/review-response` 共有）
- **編集時は必ずテストを走らせる**: `claude/.claude/rules/script-testing.md` を参照

### 通知チャンネル
- `preferredNotifChannel` は `"notifications_disabled"` を指定（組み込み通知を停止）。組み込み通知は「タスク完了・permission prompt」で発火しタイプ別フィルタ不可のため、サブエージェントのバックグラウンド起動でターンを終えた一時的アイドルでも鳴り、`idle-notify.sh` のガード（人間の入力が必要な時だけ通知）を素通りする。通知は Notification フック側（端末ベル `terminal-bell.sh`・Ping 音・Slack）に一本化し、ベルもガード済みタイミングで鳴る（過去の `"terminal_bell"` 指定はこの素通り問題があったため撤去）
- なお、さらに過去の `"iterm2"` 固定は Ghostty + tmux で通知が届かないバグ（[anthropics/claude-code#19979](https://github.com/anthropics/claude-code/issues/19979)）の回避策で、v2.1.78 の修正（Ghostty popup の tmux パススルー対応）により撤去済み
- OSC 系の通知・progress bar が tmux 越しに届くには `.tmux.conf` の `allow-passthrough all` が前提（`on` だと非表示 window のペインからの通知が破棄される）。`terminalSequence` 経由の BEL はパススルー不要（tmux 自身が bell として解釈し window flag を立てる）

### 組み込みスキルの可視性
- `skillOverrides` で組み込み `review` スキルを `"user-invocable-only"` に指定（モデルのスキル一覧から除去。ユーザーの手打ち `/review` は可能なまま）。`deep-review` と名前・機能が近接しているため、サブエージェントが「`/deep-review` を実行せよ」という指示を組み込み `review` で解決してしまい、`--worktree` が自由記述扱いになって worktree-resolution 規約に従わない worktree を即興作成する事故（2026-07-24、review-assigned-prs 経由の並列レビューで発生）を構造的に防ぐ。あわせて `deep-review` の description に PR レビュー対応を明記し、一覧上の意味マッチでも `deep-review` が PR レビューの受け皿になるようにしている

### Worktree 設定
- `worktree.baseRef: "head"` を指定。`--worktree` を持つスキル群（規約定義: `claude/.claude/skills/worktree-resolution/SKILL.md`）が「ローカル HEAD を起点に worktree を作成する」契約を保つための前提（デフォルトの `"fresh"` だと origin のデフォルトブランチから分岐するため、計画フェーズで指定された base branch を起点にできない）

## 開発ワークフロー

### Git操作の段階別コマンド
1. **コミット**: `/git-commit`
2. **PR作成**: `/git-pr`
3. **rebase（コンフリクト自動解消）**: `/git-rebase`

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
- **Stow管理ファイルの編集**: `~/.claude/`や`~/.gitconfig`等のホームディレクトリのファイルはStowによるシンボリックリンクのため、**必ずリポジトリ内のソース（`claude/.claude/`、`git/.gitconfig`等）を編集すること**。ホーム側を直接編集しない
- **Stow の tree folding 注意**: `~/.claude` は実ディレクトリで直下要素が個別 symlink のため、`claude/.claude/` 直下に新しいトップレベルディレクトリ（`scripts/` 等）を追加しても `stow -R claude`（restow）を実行するまでホーム側に現れない。参照時に "No such file or directory" で silent に失敗するため、追加時は必ず restow する
