---
name: worktree-resolution
description: PR・ブランチに対応する git worktree の解決手順（既存検索・切替・新規作成）と命名規約。/issue-handle・/review-response・/deep-review の --worktree から参照される共通規約
user-invocable: false
---

# worktree-resolution

PR・ブランチに対応する worktree を解決（既存検索・切替・新規作成）するための共通規約と手順。`/issue-handle`・`/review-response`・`/deep-review` の `--worktree` から参照される。

## 共通規約

worktree を扱う全スキルが従う契約。乖離するとスキル間で worktree を相互発見できなくなる。

- **worktree 名の計算**: branch 名から `/` を `-` に置換する
  - 例: `feature/99-add-oauth` → `feature-99-add-oauth`
- **既存 worktree の検索**: `git worktree list --porcelain` を解析し、`branch refs/heads/<branch>` が登録されている worktree を探す
- **EnterWorktree の使い分け**: 既存 worktree への切替は `EnterWorktree(path: <found-path>)`、新規作成は `EnterWorktree(name: <worktree-name>)`
- **前提**: `worktree.baseRef: "head"` 設定（`~/.claude/settings.json`、dotfiles では設定済み）

## PR worktree 解決手順

対象 PR の head branch に対応する worktree に session を切り替える手順。

1. **PR 番号確定**
   - `<pr-number>` が指定されていればそれを使用
   - 無ければ `gh pr view --json number -q .number` でカレント branch の PR を推論
     - 推論失敗時（PR が無い・複数該当など）はエラーメッセージを出して停止し、ユーザーに `<pr-number>` の明示指定を促す（誤った PR に対応しないための安全策）

2. **PR の head branch 名取得**
   - `gh pr view <PR> --json headRefName -q .headRefName`

3. **worktree 名の計算**
   - 共通規約に従い branch 名から `/` を `-` に置換

4. **既存 worktree の検索**
   - 共通規約に従い `git worktree list --porcelain` を解析
   - `branch refs/heads/<pr-branch>` が登録されている worktree を探し、見つかればそのパスを記録

5-A. **既存 worktree あり**:
   - `EnterWorktree(path: <found-path>)` で session を切替

5-B. **既存 worktree なし**（auto cleanup 後・別 PC 等）:
   - **メインリポジトリの退避**（current branch == PR head branch の時のみ実行）:
     - 理由: 後続の `git switch <pr-branch>` を worktree 内で実行する際、メインリポジトリが同 branch を checkout していると git が二重 checkout を拒否するため、先にメインリポジトリを別 branch へ退避させる
     - dirty 検出: `git status --porcelain | grep -v '^??' | head -n1` で modified/staged 変更を確認。非空なら **abort**（ユーザーに明示的なコミット/stash を促す。untracked のみは無視）
     - clean → デフォルト branch を取得して switch:
       - 取得: `git symbolic-ref refs/remotes/origin/HEAD --short 2>/dev/null | sed 's|^origin/||'`（失敗時は `gh repo view --json defaultBranchRef -q .defaultBranchRef.name` をフォールバック）
       - `git switch <default-branch>` でメインリポジトリを退避
       - ユーザーに 1 行で通知: 「メインリポジトリを <default-branch> に退避しました（worktree 作成のため）」
   - `git fetch origin <pr-branch>` で remote tracking ref を更新
   - `EnterWorktree(name: <worktree-name>)` で新規 worktree 作成
     - 結果: branch `worktree-<worktree-name>` 上の worktree、`WorktreeCreate` hook 発火
   - worktree 内で `git switch <pr-branch>` で PR の実 branch に切替
     - local に `<pr-branch>` が無い場合は git の DWIM 挙動で `origin/<pr-branch>` から自動作成（modern git 2.23+）
     - local に同名の古い `<pr-branch>` が残っている場合（前回作業の残骸等）はそちらに切り替わり、`origin/<pr-branch>` と乖離するリスクがある。`git rev-list --left-right --count <pr-branch>...origin/<pr-branch>` 等で同期状況を確認し、ローカル側に独自 commit が無ければ `git reset --hard origin/<pr-branch>` でリモートに揃える。独自 commit がある場合は警告して停止し、ユーザー判断を仰ぐ
   - `git branch -d worktree-<worktree-name>` で temp branch を削除
   - 補足: この 2 段階方式により hook 発火を確保しつつ、目的の PR branch に到達できる

6. **作業ディレクトリ確認**: worktree 内にいることを `git rev-parse --show-toplevel` で確認する

## 注意事項（`--worktree` 指定時の共通挙動）

- 並列で別の作業中に呼び出すと、session が PR の worktree に切り替わる。元の作業に戻るには別途 `EnterWorktree(path: <元のworktree>)` を呼ぶ
- 別ターミナル/別 tmux ペインで実行する運用なら、元 session は触らずに済む（並列作業の推奨運用）
- worktree を新規作成する場合、PR の head branch を fetch して checkout するため、PR ブランチ側に未 push のローカル commit があれば事前に push しておくこと
- **メインリポジトリが PR head branch を checkout 中の場合**: worktree 作成時に自動でメインリポジトリを default branch へ退避する（git の二重 checkout 禁止を回避）。dirty tree の場合は abort されるため、事前にコミット/stash しておくこと
