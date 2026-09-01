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

#### 出力 JSON の契約

```json
{
  "degraded": false,
  "default_branch": "main",
  "current_worktree": "/path/to/current",
  "candidates": {
    "worktrees": [{"path": "...", "branch": "...", "verdict": "...", "detail": "PR #123 MERGED", "is_current": false}],
    "branches": [{"branch": "...", "verdict": "...", "detail": "...", "is_current": false}]
  },
  "skipped": [{"type": "worktree|branch", "target": "...", "branch": "...", "reason": "...", "detail": "..."}],
  "detached": ["/path/to/detached-worktree"],
  "warnings": ["..."]
}
```

- **`candidates`**: 削除候補。`verdict` は判定根拠の種別:
  - `pr_merged`: PR が MERGED
  - `merged_no_pr`: PR なし & デフォルトブランチにマージ済み（main 直行運用のカバー）
  - `pr_closed`: PR が CLOSED 未マージ、かつ local head == PR head の照合済み（削除しても `gh pr checkout N` で完全復元できる）
  - OPEN の PR が併存する branch は判定対象外（in-flight として保持）
  - `detail` はそのまま一覧表示の「判定」欄に使える文字列
  - `is_current: true` はセッションが今いる worktree / チェックアウト中の branch。削除前に手順 3 の「カレント処理」が必要
  - `head_oid` は `pr_closed` のみ非空（照合済み OID）。`ccx worktree delete` が `-D` 直前に再照合するため、候補を間引く際も落とさず渡すこと
- **`skipped`**: セーフティチェックで弾かれた対象。`reason` は機械用コード、`detail` はそのまま一覧表示に使える文字列。`branch` フィールドは `type: "worktree"` のみ付与。`local_commits_beyond_pr` は「CLOSED 未マージ PR があるが PR に含まれないローカル commit を持つ」ケース。`commits_beyond_merged_pr` は「PR は MERGED だが、ローカル head がマージされた PR head と一致もその祖先（behind）もしていない」ケースで、マージ後に feature branch へ push した commit がある・乖離している・PR head がローカルに無い場合が該当する（`detail` には超過した commit の一覧が載る。PR head がローカルに無い場合のみ、一覧の代わりにその旨が載る）。`in_use_by_process` は「別プロセス（他の Claude Code セッション・シェル等）が cwd にしている worktree」で、detail のプロセス名と PID を見て手動で判断する（カレント worktree 自身はこの検査を免除され `is_current` 候補になる）
- **`detached`**: detached HEAD の worktree（branch が無く削除判定できないため別枠報告）
- **`degraded: true`**: `gh` 不通でオフライン判定のみ（PR 情報なし。`pr_closed` は PR head 照合ができないため候補に出ない）。一覧のヘッダーに「オフライン判定（PR 情報なし）」と警告を出すこと
- **`warnings`**: fetch 失敗等の注記。空でなければ一覧に併記する

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

出力 JSON の契約:

```json
{
  "removed": {"worktrees": ["..."], "branches": ["..."]},
  "failures": [{"type": "worktree|branch", "target": "...", "error": "..."}]
}
```

- 個別の削除失敗は `failures` に記録され処理は継続する（exit 0）。失敗があれば個別に報告する
- 削除の分岐（`-d`/`-D`・`--force` 不使用・使用中 worktree の拒否）は `ccx worktree delete` に集約されている。削除直前にも cwd 保持プロセスを再検査するため、収集〜承認の間に誰かが worktree に入っても削除されない

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

1. **原則 `git branch -d`**: マージ済み判定は git 自身も行うため、未マージ branch は git が拒否する二重セーフティとして機能する。`-D` は `pr_closed`（未マージクローズ）のみ（分岐はコマンドに集約済み）
2. **保護 branch は常に除外**: `main`, `master`, `develop`, リモートのデフォルトブランチ（コマンドが除外済み）
3. **メイン worktree（リポジトリ root）は対象外**: 構造上削除できない（コマンドが除外済み）
4. **PR 判定にはネットワーク必要**: `gh` が失敗する場合、スクリプトが `degraded: true` でオフライン判定（`git branch --merged` のみ）にフォールバックする。警告表示を忘れないこと
5. **未マージクローズ（`pr_closed`）の削除根拠**: local head == PR head を照合済みの branch のみ候補になる。一致していれば GitHub 側に `refs/pull/N/head` が恒久的に残るため、`gh pr checkout N` でいつでも完全復元できる（reflog より強い保証）
6. **dotfiles 特有の運用との整合**: `main` 直行運用では PR を作らないため、`merged_no_pr` 判定でカバーする
