---
name: cleanup-merged
description: マージ済み・完了済みのworktreeとlocal branchをまとめて削除し、結果を報告する。ユーザーがマージ済みブランチ・worktreeの掃除・片付けを求めたときに使用
---

# /cleanup-merged

マージ済みの worktree と local branch を検出して削除する。`/issue-handle --worktree` や `/review-response --worktree` で作成された worktree が、session 終了後も残ってゴミ化する問題に対応するためのスキル。

## 使用方法
```
/cleanup-merged  # 完了済みを削除して結果を報告
```

## 前提条件
- Git リポジトリ内で実行すること
- `gh` CLI がインストール・認証済みであること（PR 判定に使用。不通時はオフライン判定にフォールバック）
- `lsof` がインストール済みであること（使用中 worktree の検出に使用。macOS は標準搭載）

## 実行内容

削除前にユーザーへ確認を取らない。削除してよいかの判定と安全検査はコマンドが持ち、本スキルは削除後に結果を報告する。

### 1. 判定と削除

```bash
ccx worktree cleanup
```

事前準備（デフォルトブランチ取得・`git fetch`・保護 branch 除外）、マージ判定、セーフティチェック、削除を一括実行し、結果を JSON で stdout に出力する。出力の読み方は `ccx worktree cleanup --help` にある。

コマンドが非ゼロ終了した場合（git リポジトリ外・lsof 欠如等）は stderr のメッセージを提示して停止する。

### 2. カレントからの退避と再実行（`skipped` に `is_current` がある場合のみ）

セッションが立っている worktree と、メイン worktree のカレントブランチは削除されず `is_current` でスキップされる。退避してから手順 1 をもう 1 回だけ実行する:

- **worktree**: `ExitWorktree(action: "keep")` を呼んでセッションを worktree から抜く
  - 抜けられたら手順 1 を再実行する（セッションは起動元ディレクトリに戻っている）
  - no-op（EnterWorktree セッションでない）なら再実行せず、「この worktree はセッションのカレントのため削除できません。メインツリーで再実行してください」と案内する
- **branch**: `git switch <default_branch>` でカレントブランチを切り替えてから手順 1 を再実行する（`<default_branch>` は出力の `default_branch`）。失敗（未コミット変更等）したら再実行せず、その旨を報告する

再実行した場合、完了報告は 2 回の実行の結果を合わせたものにする。

### 3. 完了報告

出力の `removed`・`skipped`・`failures`・`degraded`・`warnings` を以下のフォーマットで報告する。0 件の節は省き、削除もスキップも無ければその旨だけを報告する:

```
## クリーンアップ結果
（degraded: true のとき）⚠ オフライン判定（PR 情報なし）

### 削除した Worktree (N 件)
1. <path>
   branch: <branch>（branch_removed: false なら「残存」と添える）
   判定: <detail>

### 削除した Branch (M 件)
1. <branch> — <detail>

### スキップ (K 件)
1. <target> — <detail>

### 失敗 (F 件)
1. <target> — <error>

### 警告
- <warnings の各項目>
```

- `in_use_by_process` でスキップされた対象は、`detail` のプロセス名と PID を見て手動で判断するよう添える
- 退避して再実行した `is_current` の対象は、2 回目で削除されていればスキップ欄に載せない

`WorktreeRemove` hook がプロジェクトに設定されていれば、`git worktree remove` の発火に合わせて実行される（per-worktree DB のクリーンアップ等）。

## 注意事項

削除の分岐（`-d` / `-D` の使い分け、保護 branch とメイン worktree の除外、使用中 worktree の拒否、
オフライン時の縮退）はすべてコマンドに集約されている。挙動を確かめるときは
`ccx worktree cleanup --help` を読む。削除をアドホックなシェルループで再実装しない（zsh の `path`
特殊変数事故の再発防止としてコマンドに固定している）。ここに残すのは、このリポジトリの運用に固有のことだけ:

1. **`main` 直行運用との整合**: PR を作らずに `main` へ入れる変更があるため、`merged_no_pr` 判定が
   そのぶんをカバーする。判定が出ても不安がる必要はない
2. **オフライン時は警告を必ず出す**: `degraded: true` は PR を見ていない判定なので、完了報告の冒頭に
   その旨を出さないとユーザーが根拠を誤解する
