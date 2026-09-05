# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## 概要

このリポジトリは178inaba氏の個人dotfilesコレクションです。GNU Stowを使用してシンボリックリンクベースで設定ファイルを管理しています。

セットアップ手順・パッケージ一覧は README.md を参照。

## ローカル設定

以下のローカル設定ファイルがサポートされています：
- `~/.gitconfig.local`
- `~/.zshrc.local`
- `~/.zprofile.local`

## Claude設定の詳細

### スキル

スキル一覧と各スキルの仕様（フラグ・オプション等）は `claude/.claude/skills/*/SKILL.md` を正とする。

- `deep-review` の命名: Claude Code 2.1.146 以降は組み込み `/code-review`・`/simplify` と区別するため `deep-review` としている
- `worktree-resolution` / `finding-triage` / `finding-severity` / `fresh-reader-verification` は他スキルから起動される共有知識スキルで、ユーザー直接起動は不可
- `rdsh` / `slio` / `cflio` は本リポジトリにコピーを置かず、`settings.json` の `enabledPlugins` で有効化した各 CLI リポジトリのプラグインから供給される（仕様の正は upstream の `skills/<name>/SKILL.md`、呼び出しは plugin 名で namespace され `/rdsh:rdsh` 等になる）

### コンテキストの置き場所

置き場所は「トリガーの性質」で振り分ける: 常時必要な原則 → CLAUDE.md、ファイル起点 → rules/、タスク起点 → skills/（詳細はグローバルCLAUDE.mdの「コンテキスト管理」参照）

### Hooks

各フックの設計意図（何を防ぐために存在するか）は `claude/.claude/rules/hooks-design.md` を参照（フック・`settings.json` 編集時に自動ロードされる）。

### スキル配管（ccx サブコマンド）
- スキル内の決定的処理（収集・判定・正規化）は `ccx` のサブコマンドに分離し、判断が必要な処理だけを SKILL.md の指示として残す（規約: `claude/.claude/skills/skill-authoring/SKILL.md` の「配管の分離」）
- 実装は `go/internal/<domain>/`（`issue` / `pullrequest` / `worktree` / `reviewprs` / `skill`）、cobra 定義は `go/internal/cmd/<group>.go`
- **契約の正は `ccx <cmd> --help`**。手で書かれてはおらず、型・json タグ・doc comment から `go/internal/contract/` が生成する。フィールドの意味を変えるにはそのフィールドの doc comment を編集して `go -C go generate ./internal/contract/...` を実行する
- **編集時は必ずテストを走らせる**: `claude/.claude/rules/script-testing.md` を参照

### 通知チャンネル
- `preferredNotifChannel` は `"notifications_disabled"` を指定（組み込み通知を停止）。組み込み通知は「タスク完了・permission prompt」で発火しタイプ別フィルタ不可のため、サブエージェントのバックグラウンド起動でターンを終えた一時的アイドルでも鳴り、`ccx hook idle-notify` のガード（人間の入力が必要な時だけ通知）を素通りする。通知は Notification フック側（端末ベル `ccx hook terminal-bell`・Ping 音・Slack）に一本化し、ベルもガード済みタイミングで鳴る（過去の `"terminal_bell"` 指定はこの素通り問題があったため撤去）
- OSC 系の通知・progress bar が tmux 越しに届くには `.tmux.conf` の `allow-passthrough all` が前提（`on` だと非表示 window のペインからの通知が破棄される）。`terminalSequence` 経由の BEL はパススルー不要（tmux 自身が bell として解釈し window flag を立てる）

### 組み込みスキルの可視性
- `skillOverrides` で組み込み `review` スキルを `"user-invocable-only"` に指定（モデルのスキル一覧から除去。ユーザーの手打ち `/review` は可能なまま）。`deep-review` と名前・機能が近接しているため、サブエージェントが「`/deep-review` を実行せよ」という指示を組み込み `review` で解決してしまい、`--worktree` が自由記述扱いになって worktree-resolution 規約に従わない worktree を即興作成する事故（2026-07-24、review-assigned-prs 経由の並列レビューで発生）を構造的に防ぐ。あわせて `deep-review` の description に PR レビュー対応を明記し、一覧上の意味マッチでも `deep-review` が PR レビューの受け皿になるようにしている

### Worktree 設定
- `worktree.baseRef: "head"` を指定。`EnterWorktree(name:)` で worktree を作る経路が「ローカル HEAD を起点に worktree を作成する」契約を保つための前提。現在の消費者はサブエージェントの `isolation: worktree`（ハーネス機能）のみ
- スキルの `--worktree`（issue-handle・deep-review 等の PR worktree 解決）はこの設定に依存しない。いずれも `git worktree add` による直接作成 + `EnterWorktree(path:)` 入場で、起点 ref を自分で指定するため（経緯は `go/internal/worktree/create.go` と `go/internal/worktree/resolve.go` の各ドキュメントコメント参照）

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
- **Stow の tree folding 注意**: stow 先の親（`~/.claude`・`~/.config` 等）が実ディレクトリで直下要素が個別 symlink になっている場合、その親に対応するパッケージ内ディレクトリの直下に新しい要素を追加しても `stow -R <package>`（restow）を実行するまでホーム側に現れない。**ファイル・ディレクトリを問わない**（`~/.claude/CLAUDE.md` 自身も個別 symlink であり、`claude/.claude/` 直下へのファイル追加も同じ挙動になる）。参照時に "No such file or directory" で silent に失敗するため、追加時は必ず restow する
