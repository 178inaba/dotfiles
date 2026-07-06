---
name: cleanup-merged
description: マージ済みのworktreeとlocal branchをまとめてクリーンアップ
argument-hint: [--yes] [--dry-run] [--include-closed]
disable-model-invocation: true
---

# /cleanup-merged

マージ済みの worktree と local branch を検出して削除する。`/issue-handle --worktree` や `/review-response --worktree` で作成された worktree が、session 終了後も残ってゴミ化する問題に対応するためのスキル。

## 使用方法
```
/cleanup-merged                   # 一覧表示 → 承認 → 削除
/cleanup-merged --yes             # 確認スキップで一括削除
/cleanup-merged --dry-run         # 削除候補を表示するのみ
/cleanup-merged --include-closed  # CLOSED 状態の PR も対象に含める
```

## 引数
- `--yes`: 削除前の確認プロンプトをスキップ
- `--dry-run`: 削除候補の一覧表示のみ、実際の削除は行わない
- `--include-closed`: PR が CLOSED（未マージクローズ）の worktree/branch も削除候補に含める

## 前提条件
- Git リポジトリ内で実行すること
- `gh` CLI がインストール・認証済みであること（PR 判定に使用。不通時はオフライン判定にフォールバック）
- `--include-closed` を `--yes` と併用する場合は誤削除リスクが上がるため非推奨

## 実行内容

### 1. 候補収集（スクリプト実行）

```bash
bash ~/.claude/skills/cleanup-merged/scripts/collect-candidates.sh [--include-closed]
```

事前準備（デフォルトブランチ取得・`git fetch`・保護 branch 除外）、worktree/branch の収集、マージ判定、セーフティチェックを一括実行し、JSON を stdout に出力する。判定ロジックの詳細はスクリプト本体を、挙動の担保は `claude/.claude/tests/test-collect-candidates.sh` を参照。

#### 出力 JSON の契約

```json
{
  "degraded": false,
  "default_branch": "main",
  "current_worktree": "/path/to/current",
  "candidates": {
    "worktrees": [{"path": "...", "branch": "...", "verdict": "...", "detail": "PR #123 MERGED"}],
    "branches": [{"branch": "...", "verdict": "...", "detail": "..."}]
  },
  "skipped": [{"type": "worktree|branch", "target": "...", "branch": "...", "reason": "...", "detail": "..."}],
  "detached": ["/path/to/detached-worktree"],
  "warnings": ["..."]
}
```

- **`candidates`**: 削除候補。`verdict` は判定根拠の種別:
  - `pr_merged`: PR が MERGED
  - `merged_no_pr`: PR なし & デフォルトブランチにマージ済み（main 直行運用のカバー）
  - `pr_closed`: PR が CLOSED 未マージ（`--include-closed` 指定時のみ）
  - OPEN の PR が併存する branch は判定対象外（in-flight として保持）
  - `detail` はそのまま一覧表示の「判定」欄に使える文字列
- **`skipped`**: セーフティチェックで弾かれた対象。`reason` は機械用コード、`detail` はそのまま一覧表示に使える文字列。`branch` フィールドは `type: "worktree"` のみ付与
- **`detached`**: detached HEAD の worktree（branch が無く削除判定できないため別枠報告）
- **`degraded: true`**: `gh` 不通でオフライン判定のみ（PR 情報なし）。一覧のヘッダーに「オフライン判定（PR 情報なし）」と警告を出すこと
- **`warnings`**: fetch 失敗等の注記。空でなければ一覧に併記する

スクリプトが非ゼロ終了した場合（git リポジトリ外・jq 未導入等）は stderr のメッセージを提示して停止する。

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

#### `--dry-run` 指定時
ここで終了。何も削除しない。

#### `--yes` 指定なし
ユーザーに「上記の削除を実行しますか？」と確認。承認後に削除を実行。

#### 削除実行

**Worktree**:
```bash
git worktree remove "<path>"      # --force は使わない（セーフティで未保存変更は弾いている）
git branch -d "<branch>"          # -D は使わない（マージ判定の二重チェック）
```

**Branch**（非 worktree）:
```bash
git branch -d "<branch>"
```

各削除コマンドの結果を確認し、失敗があれば個別に報告（処理は継続）。

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

1. **`git branch -d` のみ使用**: `-D` は使わない。マージ済み判定は git 自身も行うため、未マージ branch は git が拒否する二重セーフティとして機能する
2. **保護 branch は常に除外**: `main`, `master`, `develop`, リモートのデフォルトブランチ（スクリプトが除外済み）
3. **メイン worktree（リポジトリ root）は対象外**: 構造上削除できない（スクリプトが除外済み）
4. **PR 判定にはネットワーク必要**: `gh` が失敗する場合、スクリプトが `degraded: true` でオフライン判定（`git branch --merged` のみ）にフォールバックする。警告表示を忘れないこと
5. **`--include-closed` の扱い**: 未採用クローズには「気が変わって後で再開」「分割した残骸」など意図保持の可能性がある。誤削除リスクを避けるためオプトイン制
6. **dotfiles 特有の運用との整合**: `main` 直行運用では PR を作らないため、`merged_no_pr` 判定でカバーする
