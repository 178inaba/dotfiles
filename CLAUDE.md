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
- `/issue-create` - 壁打ちで固めた内容を「迷わず実装できる」Issueとして構造化・作成（情報不足は追加調査とユーザー確認で補完、粒度超過時はSub-Issues分割を提案、提示前に合意網羅チェックと会話コンテキストを持たないサブエージェントによる新規読者チェックで自己完結を検証、作成前にドラフト承認）
- `/issue-handle` - Issueの調査から実装完了まで対応（Planモードで対話型、`--file`でファイルから仕様読み込み、`--worktree`で専用worktreeに隔離して並列開発可能、完了時に独立セッションで `/deep-review` を実行）
- `/review-response` - GitHubレビューコメントの自動対応（`<pr-number>`でPR指定、`--worktree`で対象PRのworktreeに切替/作成、`--dry-run`で確認のみ）
- `/deep-review` - コード差分を詳細にレビュー（ベースブランチは自動判定、`--issue`でIssue連携、`<pr-number> --worktree`で対象PRのworktreeに切替/作成して並列レビュー）。Claude Code 2.1.146以降は組み込み `/code-review`・`/simplify` と区別するため `deep-review` 命名
- `/check-plan-compliance` - 現計画とプロジェクトCLAUDE.md・リンク先文書との準拠チェック後、計画修正と plan モード復帰まで自動実行
- `/cleanup-merged` - マージ済みのworktreeとlocal branchをまとめてクリーンアップ（`--dry-run`で確認のみ、`--yes`で確認スキップ、`--include-closed`でCLOSED状態のPRも対象）
- `/review-assigned-prs` - 自分にレビュー依頼が来ているPRのうち Bot 以外のレビューが未着のものを `/deep-review <PR番号> --worktree --no-autofix` で並列レビュー（`/loop 5m /review-assigned-prs` で常駐運用推奨）
- `/bestpractice` - プロジェクト慣習を無視した一般的なベストプラクティスを確認
- `/troubleshooting` - エラー調査・デバッグの具体的手法（エラー調査タスク時に自動ロードされる知識スキル）
- `/test-implementation` - テストコードの品質評価3原則（テスト作成・レビュータスク時に自動ロードされる知識スキル。旧 `context/test-implementation.md` から移行）
- `/github-sub-issues` - GitHub Sub-Issuesの作成・リンク手順（Sub-Issue操作タスク時に自動ロードされる知識スキル）
- `worktree-resolution` - PR・ブランチに対応するworktreeの解決手順と命名規約（`--worktree` を持つスキルから参照される共有知識スキル。ユーザー直接起動は不可）

### 設定ファイル構造
```
~/.claude/
├── CLAUDE.md           # グローバル基本方針（常時ロード）
├── skills/             # スキル定義（タスク起点で自動ロード）
├── rules/              # ルール（frontmatterのpaths globに該当するファイルを扱うときだけ遅延読込）
├── context/            # 詳細コンテキスト（CLAUDE.mdから@importで常時ロード）
├── scripts/            # スキル横断で共有するスクリプト（fetch-pr-context.sh等）
├── hooks/              # イベントフック（通知・事故防止等）
│   └── tests/          # フックのリグレッションテスト
├── tests/              # フック以外のスクリプトのリグレッションテスト（statusline・スキルスクリプト等）
├── settings.json       # Claude Code設定
└── statusline.sh       # ステータスライン表示スクリプト
```

コンテキストの置き場所は「トリガーの性質」で振り分ける: 常時必要な原則 → CLAUDE.md、ファイル起点 → rules/、タスク起点 → skills/（詳細はグローバルCLAUDE.mdの「コンテキスト管理」参照）

### Hooks
- `gh-write-guard.sh` (PreToolUse) — `gh` の書き込み系サブコマンドで `-R/--repo` を必須化し、別リポジトリへ `cd` した状態で意図しないリポジトリに Issue/PR を作成する事故を防ぐ。また複数行の本文を `--body`/`-b` で渡すことをブロックして `--body-file` へ誘導し、引用符レイヤの重なりによる誤エスケープで本文にリテラルの `\` が残る事故を防ぐ。さらに本文中に項番とみられる素の `#N` の連番を検出した場合もブロックし、無関係な Issue/PR へ参照通知（mentioned 表示）が飛ぶ事故を防ぐ（検出条件・除外・fail-open の詳細はフックのヘッダーコメント参照）
- `worktree-edit-guard.sh` (PreToolUse) — session cwd が linked worktree 内なのに Edit/Write/NotebookEdit の対象パスが同一リポジトリの worktree 外（メインツリー本体・別 worktree）を指す場合にブロックし、worktree 側の対応パスへ誘導する。EnterWorktree は session cwd を切り替えるがツールに渡す絶対パスは自動変換されないため、切替前の調査で Read したメインツリー絶対パスを Edit に流用してメインツリーを誤修正する事故を防ぐ（Read はどちらのツリーでも同じ内容が返るため直前まで気付けない）。所属ツリーは `git worktree list` で分類し、どのツリーにも属さないパス（scratchpad 等）は対象外。dir symlink 経由でツリー内を指すパス（stow の `~/.claude/hooks/...` 等）は物理化されブロック対象になるが、ファイル単体の symlink は解決せず素通りする（既知の限界）
- `start-caffeinate.sh` (UserPromptSubmit, PreToolUse, SubagentStart) / `stop-caffeinate.sh` (Stop, Notification, SessionEnd, SubagentStop) — Claude が作業中の間だけ macOS のスリープを抑止する。caffeinate は30分リース（`-t 1800`）付きで起動し、ツール呼び出しのたびに kill→再起動でリースを更新する。Esc 中断・API エラー等 Stop フックを経ない終了では更新が止まって自己失効するため、caffeinate が残留して眠らなくなる事故を防ぐ。サブエージェント（バックグラウンド実行され、親ターン終了の Stop 後も動き続ける）には SubagentStart で per-agent caffeinate（`/tmp/claude-caffeinate-${session_id}-agent-${agent_id}.pid`）を起動し、SubagentStop で `.done` にリネームして親の次の Stop で回収する（完了直後に親が結果を読んでいる間の抑止空白を防ぐ。稼働中の `.pid` は Stop でも殺さない）。例外として Remote Control 接続中（`CLAUDE_CODE_BRIDGE_SESSION_ID` あり、Claude Code v2.1.199+）は、ホストのスリープで約10分後にリモートセッションがタイムアウトするため、セッションの caffeinate を `-t` なしで起動し SessionEnd 以外では停止せず返信待ちの間も抑止を維持する。caffeinate は Claude 本体プロセスを `-w` で watch して起動するため、クラッシュ・SIGKILL 等フックを経由しない終了でも残留しない
- **編集時は必ずテストを走らせる**: 対応表・理由・テストの設計制約は `claude/.claude/rules/script-testing.md` を参照

### スキルスクリプト
- スキル内の決定的処理（収集・判定・正規化）はスクリプトに分離し、判断が必要な処理だけを SKILL.md の指示として残す（規約: `claude/.claude/rules/skill-authoring.md` の「スクリプト同梱パターン」）
  - `skills/cleanup-merged/scripts/collect-candidates.sh` — 削除候補の収集・マージ判定・セーフティチェック
  - `skills/review-assigned-prs/scripts/list-pending-reviews.sh` — Bot 以外のレビュー未着 PR の候補収集・判定
  - `skills/review-assigned-prs/scripts/ensure-clone.sh` — レビュー用 clone dir の ensure（未 clone は clone、既存は fetch）
  - `skills/review-assigned-prs/scripts/verify-posted-reviews.sh` — サブエージェント完了報告後のレビュー投稿検証
  - `scripts/fetch-pr-context.sh` — PR コンテキスト一括取得（`/deep-review`・`/review-response` 共有）
- **編集時は必ずテストを走らせる**: `claude/.claude/rules/script-testing.md` を参照

### 通知チャンネル
- `preferredNotifChannel` は `"iterm2"` を指定。`"auto"`・`"ghostty"` は Ghostty + tmux 環境で通知が届かない既知バグ（[anthropics/claude-code#19979](https://github.com/anthropics/claude-code/issues/19979)）の回避策
- upstream 修正後は `"ghostty"` または `"auto"` に戻す

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
