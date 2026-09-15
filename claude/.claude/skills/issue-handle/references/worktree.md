# `--worktree` の差分

引数に `--worktree` がある実行が読む。SKILL.md の事前準備・Planモード内・注意事項に対する差分を持つ。

## 事前準備 Step 3

- Issue 番号に対応する既存 worktree を判定し、残骸なら削除する（判定条件と出力の読み方は `ccx worktree detect --help`）:
  ```bash
  ccx worktree detect <issue-number> --base <base-branch>
  ```
- `status: none` / `removed` → Step 4 へ進む（`removed` は Plan モード冒頭の報告に併記する）
- `status: kept` → **停止**し、`worktree_path`・`branch`・`reason` を報告してユーザー判断を仰ぐ
- 非ゼロ exit → stderr を提示して停止

## 事前準備 Step 5

- 同梱スクリプトで worktree と branch を作成する:
  ```bash
  ccx worktree create <worktree-name> <branch> <base-branch>
  ```
  （`<worktree-name>` は Step 4 の sanitized 名、`<branch>` は Step 4 の完全形式のブランチ名）
- 出力の読み方は `ccx worktree create --help` にある
  - `status: ok` → Step 6 へ。`warnings[]` が空でなければ報告に併記し、`start_ref` がローカル base の場合はその旨も報告する
  - `status: branch_exists` / `path_exists` → **停止**してユーザー判断を仰ぐ（破棄はユーザー確認なしに行わない）
  - 非ゼロ exit（base 不在等）→ stderr を提示して abort

## 事前準備 Step 6

- `EnterWorktree(path: <worktree_path>)` で session を worktree に切り替える（`<worktree_path>` は Step 5 の出力値。`name:` を使わない規約は `worktree-resolution` の「共通規約」）
- **失敗時のリカバリ**: Step 5 で作成した worktree・branch を片付けて（`git worktree remove <worktree_path>` + `git branch -D <branch>`）、ユーザーに失敗を通知して abort する

## Planモード内

Plan モード冒頭でユーザーに 1 行報告する（Step 3 が `removed` だった場合は括弧内に「Issue の残骸 worktree `<worktree_path>` を削除済み」を併記する）。

```
作業 worktree を作成し、branch `<branch>` で作業します（事前準備で完了済）。名前を変更したい場合はご指摘ください。
```

名前変更を希望されたら worktree 破棄 → 再作成で対応する: Plan モードを抜けて `ExitWorktree(action: "keep")` でメインツリーへ戻り、Step 6 の失敗時リカバリと同じ手順で破棄 → Step 4-6 を新しい名前で再実行 → 改めて EnterPlanMode。

## 注意事項

- 並列で複数 issue を進める場合、issue 1 つにつき 1 つの Claude session が必要
- スクリプト作成で失われるもの（`WorktreeCreate` hook の発火・終了時の自動クリーンアップ判定）と `.worktreeinclude` は `worktree-resolution` の「共通規約」。hook で worktree 環境を構築するプロジェクト（非 git VCS、per-worktree の DB 分離等）は本スキルの `--worktree` の対象外で、必要なら hook 相当のセットアップを手動実行する
- 回収: マージ後は `/cleanup-merged`、手動で片付ける場合は `git worktree remove <path>` + `git branch -d <branch>`
