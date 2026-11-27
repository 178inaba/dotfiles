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

削除前にユーザーへ確認を取らない。削除してよいかの判定と安全検査はコマンドが持ち、本スキルは削除後に結果を 1 回報告する。

### 1. 候補収集

```bash
ccx worktree collect
```

事前準備（デフォルトブランチ取得・`git fetch`・保護 branch 除外）、worktree/branch の収集、マージ判定、セーフティチェックを一括実行し、JSON を stdout に出力する。候補と判断した理由は各要素の `verdict` / `detail` に載る。

出力の読み方は `ccx worktree collect --help` にある。本スキルがそれに対して行うこと:

- `detail` はそのまま完了報告の各行に使う（手順 4）
- `is_current: true` の候補があれば、削除実行の前に「カレント処理」を行う（手順 2）
- `head_oid` はカレント処理で候補を外す際も落とさず渡す（`ccx worktree delete` が `-D` の直前に再照合するため）
- `in_use_by_process` でスキップされた対象は、`detail` のプロセス名と PID を見て手動で判断する
- `degraded: true` なら完了報告の冒頭に「オフライン判定（PR 情報なし）」と警告を出す
- `warnings` が空でなければ完了報告に併記する

削除候補（`candidates` の worktree・branch）が 0 件の場合は、その旨を報告して終了する（スキップ・オフライン判定の警告・`warnings` があればあわせて報告する）。

コマンドが非ゼロ終了した場合（git リポジトリ外・lsof 欠如等）は stderr のメッセージを提示して停止する。

### 2. カレント処理（`is_current: true` の候補がある場合のみ）

- **worktree 候補**: `ExitWorktree(action: "keep")` を呼んでセッションを worktree から抜く
  - 抜けられたら、その候補もそのまま削除実行に含める（セッションは起動元ディレクトリに戻っている）
  - no-op（EnterWorktree セッションでない）なら、その候補を削除セットから外し「この worktree はセッションのカレントのため削除できません。メインツリーで再実行してください」と案内（`ccx worktree delete` は cwd 保持プロセスの居る worktree を拒否するため、外し忘れも failures に出る）
- **branch 候補**: `git switch <default_branch>` でカレントブランチを切り替える。失敗（未コミット変更等）したらその候補を削除セットから外す

外した候補は完了報告のスキップ欄に理由付きで載せる。外した結果、削除セットが空になったら手順 3 を行わず完了報告に進む。

### 3. 削除実行

```bash
ccx worktree delete < <(手順 1 の出力 JSON)
```

手順 1 の出力 JSON をそのまま stdin に渡す。カレント処理で外した候補があれば jq でその要素だけを `candidates` から取り除いて渡す（**アドホックなシェルループで削除を再実装しない**。zsh の `path` 特殊変数事故の再発防止としてコマンドに固定している）。

出力の読み方は `ccx worktree delete --help` にある。個別の削除失敗は処理を止めない。

`WorktreeRemove` hook がプロジェクトに設定されていれば、`git worktree remove` の発火に合わせて実行される（per-worktree DB のクリーンアップ等）。

### 4. 完了報告

`ccx worktree delete` の `removed` はパスとブランチ名だけを持つので、各行の `detail` は手順 1 の出力から引く。以下のフォーマットで報告し、0 件の節は省く:

```
## クリーンアップ結果
（degraded: true のとき）⚠ オフライン判定（PR 情報なし）

### 削除した Worktree (N 件)
1. <path>
   branch: <branch>（削除できなかった場合は「残存」と添える）
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

- 削除した Worktree: `removed.worktrees` の各パス
- 削除した Branch: `removed.branches` のうち、手順 1 の `candidates` の branch 側にあったもの（worktree 候補のブランチは Worktree 節の `branch:` で示す）
- スキップ: 手順 1 の `skipped` と、カレント処理で外した候補
- 失敗: `failures` の各要素を 1 件ずつ、`error` を添えて

## 注意事項

削除の分岐（`-d` / `-D` の使い分け、保護 branch とメイン worktree の除外、使用中 worktree の拒否、
オフライン時の縮退）はすべてコマンドに集約されている。挙動を確かめるときは
`ccx worktree collect --help` と `ccx worktree delete --help` を読む。ここに残すのは、このリポジトリの
運用に固有のことだけ:

1. **`main` 直行運用との整合**: PR を作らずに `main` へ入れる変更があるため、`merged_no_pr` 判定が
   そのぶんをカバーする。判定が出ても不安がる必要はない
2. **オフライン時は警告を必ず出す**: `degraded: true` は PR を見ていない判定なので、完了報告の冒頭に
   その旨を出さないとユーザーが根拠を誤解する
