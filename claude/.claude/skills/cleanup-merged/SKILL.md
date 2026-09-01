---
name: cleanup-merged
description: マージ済みのworktreeとlocal branchをまとめてクリーンアップ。ユーザーがマージ済みブランチ・worktreeの掃除・片付けを求めたときに使用（既定で一覧提示→承認後に削除）
argument-hint: "[--yes]"
---

# /cleanup-merged

マージ済みの worktree と local branch を検出して削除する。`/issue-handle --worktree` や `/review-response --worktree` で作成された worktree が、session 終了後も残ってゴミ化する問題に対応するためのスキル。

## 使用方法
```
/cleanup-merged        # 一覧表示 → 承認 → 削除
/cleanup-merged --yes  # 確認スキップで一括削除
```

## 引数
- `--yes`: 削除前の確認プロンプトをスキップ（削除候補の確認だけしたい場合は、フラグなしで実行して確認プロンプトを拒否すればよい）

## 前提条件
- Git リポジトリ内で実行すること
- `gh` CLI がインストール・認証済みであること（PR 判定に使用。不通時はオフライン判定にフォールバック）
- `lsof` がインストール済みであること（使用中 worktree の検出に使用。macOS は標準搭載）

## 実行内容

### 1. 候補収集

```bash
ccx worktree collect
```

事前準備（デフォルトブランチ取得・`git fetch`・保護 branch 除外）、worktree/branch の収集、マージ判定、セーフティチェックを一括実行し、JSON を stdout に出力する。候補と判断した理由は各要素の `verdict` / `detail` に載る。

出力の読み方は `ccx worktree collect --help` にある。本スキルがそれに対して行うこと:

- `detail` はそのまま一覧の「判定」欄に使う（手順 2）
- `is_current: true` の候補があれば、削除実行の前に「カレント処理」を行う（手順 3）
- `head_oid` は候補を間引く際も落とさず渡す（`ccx worktree delete` が `-D` の直前に再照合するため）
- `in_use_by_process` でスキップされた対象は、`detail` のプロセス名と PID を見て手動で判断する
- `degraded: true` なら一覧のヘッダーに「オフライン判定（PR 情報なし）」と警告を出す
- `warnings` が空でなければ一覧に併記する

コマンドが非ゼロ終了した場合（git リポジトリ外・lsof 欠如等）は stderr のメッセージを提示して停止する。

### 2. 削除候補の一覧表示

JSON の内容を以下のフォーマットで報告:

```
## 削除候補

### Worktree (N 件)
1. <path>
   branch: <branch>
   判定: <detail>

### Branch (M 件)
1. <branch> — <detail>

### スキップ (K 件)
1. <target> — <detail>
```

削除候補が 0 件の場合はその旨を報告して終了。

### 3. 確認・削除

#### `--yes` 指定なし
ユーザーに「上記の削除を実行しますか？」と確認。承認後に削除を実行。

#### カレント処理（`is_current: true` の候補がある場合のみ、削除実行の前に）

- **worktree 候補**: `ExitWorktree(action: "keep")` を呼んでセッションを worktree から抜く
  - 抜けられたら、その候補もそのまま削除実行に含める（セッションは起動元ディレクトリに戻っている）
  - no-op（EnterWorktree セッションでない）なら、その候補を削除セットから外し「この worktree はセッションのカレントのため削除できません。メインツリーで再実行してください」と案内（`ccx worktree delete` は cwd 保持プロセスの居る worktree を拒否するため、外し忘れも failures に出る）
- **branch 候補**: `git switch <default_branch>` でカレントブランチを切り替える。失敗（未コミット変更等）したらその候補を削除セットから外して報告

#### 削除実行

```bash
ccx worktree delete < <(承認済み候補の JSON)
```

手順 1 の出力 JSON をそのまま stdin に渡す。ユーザーが一部のみ承認した場合は jq で `candidates` を間引いて渡す（**アドホックなシェルループで削除を再実装しない**。zsh の `path` 特殊変数事故の再発防止としてコマンドに固定している）。

出力の読み方は `ccx worktree delete --help` にある。個別の削除失敗は処理を止めないので、`failures`
が空でなければ 1 件ずつ報告する。

`WorktreeRemove` hook がプロジェクトに設定されていれば、`git worktree remove` の発火に合わせて実行される（per-worktree DB のクリーンアップ等）。

### 4. 完了報告

```
削除完了:
- Worktree: N 件
- Branch: M 件
- スキップ: K 件（理由は上記参照）
- 失敗: F 件（あれば詳細を再掲）
```

## 注意事項

削除の分岐（`-d` / `-D` の使い分け、保護 branch とメイン worktree の除外、使用中 worktree の拒否、
オフライン時の縮退）はすべてコマンドに集約されている。挙動を確かめるときは
`ccx worktree collect --help` と `ccx worktree delete --help` を読む。ここに残すのは、このリポジトリの
運用に固有のことだけ:

1. **`main` 直行運用との整合**: PR を作らずに `main` へ入れる変更があるため、`merged_no_pr` 判定が
   そのぶんをカバーする。判定が出ても不安がる必要はない
2. **オフライン時は警告を必ず出す**: `degraded: true` は PR を見ていない判定なので、一覧のヘッダーに
   その旨を出さないとユーザーが根拠を誤解する
