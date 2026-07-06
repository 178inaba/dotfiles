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
- `gh` CLI がインストール・認証済みであること（PR 判定に使用）
- `--include-closed` を `--yes` と併用する場合は誤削除リスクが上がるため非推奨

## 実行内容

### 1. 事前準備

1. **デフォルトブランチ取得**:
   ```bash
   default_branch=$(git symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's@^refs/remotes/origin/@@')
   # fallback
   [ -z "$default_branch" ] && default_branch="main"
   ```

2. **デフォルトブランチを最新化**:
   ```bash
   git fetch origin "$default_branch"
   ```
   後段の (b) 判定で参照する `origin/$default_branch` の merge base を最新化するため。`fetch` が失敗した場合は警告のみで続行し、後段の (b) 判定で「ローカル `$default_branch` が stale な可能性あり」と注記する（dotfiles の main 直行運用で別マシン経由のマージを取りこぼさないための処置）。

3. **メイン worktree のパス取得**（除外対象の特定用）:
   ```bash
   main_worktree=$(git worktree list --porcelain | awk '/^worktree / {print $2; exit}')
   ```

4. **カレント session のパス取得**（削除不可対象の特定用）:
   ```bash
   current_worktree=$(git rev-parse --show-toplevel)
   ```

5. **保護 branch のリスト**: `main`, `master`, `develop`, `default_branch` を保護対象として常に除外

### 2. 対象の収集

#### Worktree 一覧
```bash
git worktree list --porcelain
```
を解析し、`worktree <path>` と `branch refs/heads/<branch>` をペアで取得。
- メイン worktree（`main_worktree` と一致）は除外
- detached HEAD の worktree は別途報告して除外（branch が無いと削除判定できないため）

#### 非 worktree branch 一覧
```bash
git branch --format='%(refname:short)'
```
で取得し、以下を除外:
- 保護 branch（`main`, `master`, `develop`, `default_branch`）
- worktree でチェックアウトされている branch（重複防止）
- `git branch --show-current` で取得したカレント branch（削除不可）

### 3. マージ判定

各対象（worktree / branch）について、以下のいずれかに該当するか確認。

**前提**: `gh pr list` が非ゼロ終了する場合（ネットワーク断・認証失効等）は (a) と (c) をスキップし、(b) のみで判定する。ヘッダーに「オフライン判定（PR 情報なし）」と警告を出すこと。

#### (a) PR が MERGED
```bash
gh pr list --head "<branch>" --state merged --json number,mergedAt --limit 1 -R <owner/repo>
```
結果が空でなければ「PR #<number> MERGED」として判定。

#### (b) PR なし & デフォルトブランチにマージ済み
```bash
# PR が存在しないことを確認
gh pr list --head "<branch>" --state all --json number --limit 1 -R <owner/repo>
# デフォルトブランチにマージ済みか確認（origin 側を参照して local stale を回避、worktree branch の "+" プレフィックスも捕捉）
git branch --merged "origin/$default_branch" | awk '{print $NF}' | grep -Fx "<branch>"
```
両方を満たす場合に「main 直行マージ済み」として判定。

`awk '{print $NF}'` で先頭マーカー（`*` カレント / `+` 他 worktree checked out / 空白 その他）を除去してから `grep -Fx` で完全一致比較する。`git branch --merged` の出力形式は `man git-branch` を参照（worktree でチェックアウト中の branch は `+` プレフィックスとなり、`^[ *]+` の正規表現ではマッチしないため）。

#### (c) PR が CLOSED（`--include-closed` 指定時のみ）
```bash
gh pr list --head "<branch>" --state closed --json number,state,mergedAt -R <owner/repo> \
  --jq '[.[] | select(.mergedAt == null)] | first'
```
`gh pr list --state closed` は MERGED な PR も返すため、`mergedAt == null` で「クローズされたが未マージ」のみに絞り込む。結果が空でなければ「PR #<number> CLOSED（未マージ）」として判定。

いずれにも該当しなければ削除候補から除外（in-flight として保持）。

### 4. セーフティチェック

削除候補に対して以下を確認し、引っかかったものは「スキップ」として別枠に分類:

#### Worktree
- **未コミット変更**: `( cd <path> && git status --porcelain )` で出力があれば skip
- **未 push commit**: `( cd <path> && git log @{u}..HEAD --oneline 2>/dev/null )` で出力があれば skip
- **カレント session の worktree**: パスが `current_worktree` と一致すれば skip

#### Branch
- **未 push commit**: `git log <branch>@{u}..<branch> --oneline 2>/dev/null` で出力があれば skip
- **upstream 未設定 & 自前 commit あり**: `git rev-parse --abbrev-ref <branch>@{u}` が失敗かつ `git log <default_branch>..<branch> --oneline` が非空なら skip（ローカル限定 branch で `--merged` 判定をすり抜けるケースの保険）

### 5. 削除候補の一覧表示

以下のフォーマットで報告:

```
## 削除候補

### Worktree (N 件)
1. <path>
   branch: <branch>
   判定: PR #123 MERGED

2. <path>
   branch: <branch>
   判定: <default_branch> にマージ済み（PRなし）

### Branch (M 件)
1. <branch> — PR #456 MERGED
2. <branch> — <default_branch> にマージ済み（PRなし）

### スキップ (K 件)
1. <path> — 未コミット変更あり
2. <branch> — 未 push commit あり（N 件）
3. <path> — カレント session の worktree
```

削除候補が 0 件の場合はその旨を報告して終了。

### 6. 確認・削除

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

### 7. 完了報告

```
削除完了:
- Worktree: N 件
- Branch: M 件
- スキップ: K 件（理由は上記参照）
- 失敗: F 件（あれば詳細を再掲）
```

## 注意事項

1. **`git branch -d` のみ使用**: `-D` は使わない。マージ済み判定は git 自身も行うため、未マージ branch は git が拒否する二重セーフティとして機能する
2. **保護 branch は常に除外**: `main`, `master`, `develop`, リモートのデフォルトブランチ
3. **メイン worktree（リポジトリ root）は対象外**: 構造上削除できない
4. **PR 判定にはネットワーク必要**: `gh pr list` が失敗する場合はオフライン判定（`git branch --merged` のみ）にフォールバックして警告を出す
5. **`--include-closed` の扱い**: 未採用クローズには「気が変わって後で再開」「分割した残骸」など意図保持の可能性がある。誤削除リスクを避けるためオプトイン制
6. **dotfiles 特有の運用との整合**: `main` 直行運用では PR を作らないため、(b) 判定でカバーする
