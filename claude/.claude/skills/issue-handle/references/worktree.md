# `--worktree` の差分

引数に `--worktree` がある実行が読む。SKILL.md の各ステップに対する差分をフェーズとステップ番号ごとに持つ。

## 事前準備

**流れの要約**: 調査 (0) → base 確定・fetch (1-2) → 既存 worktree の残骸判定 (3、残骸なら削除) → 名前確定・worktree 作成・切替 (4-6) → Plan モード (7)。worktree はベースブランチから直接作成し、メインツリーの状態（HEAD・working tree）には一切触れないため、Plan モード中もメインツリーで並列作業可能。

## 事前準備 Step 3

**既存 worktree の残骸判定**（Issue番号指定時のみ）
- Issue 番号に対応する既存 worktree を判定し、残骸なら削除する（判定条件と出力の読み方は `ccx worktree detect --help`）:
  ```bash
  ccx worktree detect <issue-number> --base <base-branch>
  ```
- `status: none` / `removed` → Step 4 へ進む（`removed` は Plan モード冒頭の報告に併記する）
- `status: kept` → **停止**し、`worktree_path`・`branch`・`reason` を報告してユーザー判断を仰ぐ
- 非ゼロ exit → stderr を提示して停止
- 補足: `--file` 指定時（Issue 番号なし）は worktree 名の予測が安定しないため、本ステップはスキップする。実装フェーズの作業ブランチ確定ステップでの衝突検出フォールバックでカバーする

## 事前準備 Step 5

**worktree 作成**
- 同梱スクリプトで worktree と branch を作成する:
  ```bash
  ccx worktree create <worktree-name> <branch> <base-branch>
  ```
  （`<worktree-name>` は Step 4 の sanitized 名、`<branch>` は Step 4 の完全形式のブランチ名）
- 出力の読み方は `ccx worktree create --help` にある
  - `status: ok` → Step 6 へ。`warnings[]` が空でなければ報告に併記し、`start_ref` がローカル base の場合はその旨も報告する
  - `status: branch_exists` / `path_exists` → **停止**してユーザー判断を仰ぐ（過去作業の残骸の可能性があり、破棄はユーザー確認なしに行わない。Step 3 の判定に掛からない片割れ残骸 — branch だけ・ディレクトリだけ — が典型）
  - 非ゼロ exit（base 不在等）→ stderr を提示して abort

## 事前準備 Step 6

**EnterWorktree 実行**
- `EnterWorktree(path: <worktree_path>)` で session を worktree に切り替える（`<worktree_path>` は Step 5 の出力値）
  - `EnterWorktree(name:)` を使わないのは base branch を指定できないため。path 入場のため session は worktree の owner にならず、終了時の自動クリーンアップ判定は働かない（後始末は下記「注意事項」参照）
- **失敗時のリカバリ**: session はまだメインツリーの cwd。Step 5 で作成した worktree・branch を片付けて（`git worktree remove <worktree_path>` + `git branch -D <branch>`。作成直後でコミット・変更なしのため安全）、ユーザーに失敗を通知して abort（原因究明はユーザーに委ねる）

## Planモード内

Plan モード冒頭でユーザーに 1 行報告する（Step 3 が `removed` だった場合は括弧内に「Issue の残骸 worktree `<worktree_path>` を削除済み」を併記する）。

```
作業 worktree を作成し、branch `<branch>` で作業します（事前準備で完了済）。名前を変更したい場合はご指摘ください。
```

名前変更を希望されたら worktree 破棄 → 再作成で対応する: Plan モードを抜けて `ExitWorktree(action: "keep")` でメインツリーへ戻り、Step 6 の失敗時リカバリと同じ手順で破棄 → Step 4-6 を新しい名前で再実行 → 改めて EnterPlanMode。

## 注意事項

- 並列で複数 issue を進める場合、issue 1 つにつき 1 つの Claude session（別ターミナル/別 tmux ペイン）が必要
- worktree はベースブランチから直接作成し、メインツリーの状態（HEAD・working tree）には一切触れない。Plan モード中もメインツリーで並列の別作業が可能
- **branch 名はブランチ名（完全形式、例: `feature/99-add-oauth`）をそのまま使う**。PR の head branch もこの形式
- `.env` 等の gitignored ファイルは各プロジェクト個別に `.worktreeinclude` で列挙する（コピーは `ccx worktree create` がネイティブ挙動を再現）
- **`WorktreeCreate` hook は発火しない**（`git worktree add` 直接作成のため）。hook で worktree 環境を構築するプロジェクト（非 git VCS、per-worktree の DB 分離等）は本スキルの `--worktree` の対象外で、必要なら hook 相当のセットアップを手動実行する（スキル本体は DB を意識しない）
- クリーンアップ: path 入場のため session は worktree の owner にならず、終了時の自動クリーンアップ判定（変更なし→自動削除等）は働かない。マージ後の回収は `/cleanup-merged`、手動で片付ける場合は `git worktree remove <path>` + `git branch -d <branch>`
