---
name: worktree-resolution
description: PR・ブランチに対応する git worktree の解決手順（既存検索・切替・新規作成）と命名規約、およびレビュー系スキル共有の PR head 鮮度確認サブ手順
user-invocable: false
---

# worktree-resolution

PR・ブランチに対応する worktree を解決（既存検索・切替・新規作成）するための共通規約と手順。`/issue-handle`・`/review-response`・`/deep-review` の `--worktree` から参照される。PR を対象にレビュー・修正を行うスキルが共有する鮮度確認サブ手順もここに置く（こちらは worktree の有無に関わらず参照される）。

決定的な配管（PR 番号確定・worktree 名計算・既存検索・同期・退避・作成後の switch）は `scripts/` の2本が実行し、AI 判断に残るのは作成・切替プリミティブの分岐（session 状態はスクリプトから観測できない）と、停止 status 時のユーザー対応のみ。

## 共通規約

worktree を扱う全スキルが従う契約。乖離するとスキル間で worktree を相互発見できなくなる。

- **worktree 名の計算**: branch 名から `/` を `-` に置換する
  - 例: `feature/99-add-oauth` → `feature-99-add-oauth`
  - **必ず sanitize してから `EnterWorktree(name:)` に渡す**。`/` を含む値を渡すと Claude Code 実装により `+` に置換され、以降の既存 worktree 検索パターンから外れて再開検出に失敗する（2026-07-06 実測、公式未文書挙動）
- **既存 worktree の検索**: `git worktree list --porcelain` を解析し、`branch refs/heads/<branch>` を checkout 中の linked worktree を探す（メイン worktree は除外 — メインが対象 branch を checkout 中のケースは退避で扱う）。PR worktree 解決では resolve スクリプトが実行する
- **EnterWorktree を優先する**: `EnterWorktree` は組み込みのライフサイクル統合点（`.worktreeinclude` コピー・終了時の自動クリーンアップ判定・WorktreeCreate hook 発火）であることに加え、session の `workspace.project_dir` を worktree へ切り替える（Bash `cd` はプロセス cwd を変えるだけで、project_dir 起点で解決される機構はそのままになる）。Bash `cd` 代替は以下の構造的に不可能な文脈に限る:
  - **サブエージェント内では新規作成（`name:`）は一律拒否される**（"EnterWorktree cannot create a worktree from a subagent with a cwd override" エラー。明示的な `isolation`・`cwd` 指定がないサブエージェントでも発生する — 2026-07-07 実測。公式仕様上サブエージェントは `path` 形式のみ・対象は `.claude/worktrees/` 配下限定）→ 作成は `create-fallback` サブコマンドで代替する
  - **`path:` 切替もエージェント起動時に固定されたプロセス cwd で「現在のリポジトリ」を判定する**（Bash の `cd` では変わらない）。対象リポジトリ外の cwd で起動されたサブエージェント（/review-assigned-prs の clone dir 構成等）では `path:` 切替も "the current directory is not in a git repository" で失敗する（2026-07-07 実測）。該当する場合は EnterWorktree を試行せず Bash `cd` で代替する
- **Bash `cd` 代替は毎回前置する**: 各 Bash 呼び出しに `cd <対象パス> && ...` を前置して作業する（worktree 作成前のリポジトリ操作は対象リポジトリへ、作成後の作業は worktree へ。git 操作は `git -C <対象パス>` でも可、ファイル操作は絶対パスを使用）。毎回前置が必須なのは公式仕様のため: サブエージェントの Bash は cwd を呼び出し間で持ち越さない（許可ディレクトリ内でも起動時 cwd に戻る — 2026-07-24 実測確認）。メインセッションでも project directory / additionalDirectories 外への `cd` は自動リセットされる
- **EnterWorktree 後の絶対パス**: session cwd は worktree に切り替わるが、Edit/Write に渡す絶対パスは自動変換されない。切替**前**に Read したメインツリー絶対パスを Edit/Write に流用しない（機械的強制: `~/.claude/hooks/worktree-edit-guard.sh` がブロックする）
- **前提**: `worktree.baseRef: "head"` 設定（`~/.claude/settings.json`、dotfiles では設定済み）

## PR worktree 解決手順

対象 PR の head branch に対応する worktree に session を切り替える手順。

1. **解決**（対象リポジトリ内で実行。`<pr-number>` 省略時はカレント branch の PR を推論）:
   ```bash
   bash ~/.claude/skills/worktree-resolution/scripts/resolve-pr-worktree.sh resolve [<pr-number>]
   ```
   出力 JSON のうち本手順で使うフィールド（契約の正はスクリプトヘッダー）: `status` / `action` / `worktree_path` / `worktree_name` / `head_ref` / `evacuated` / `warnings[]`
   - `status` が `ok` 以外（`behind_dirty` / `diverged` / `evacuation_dirty`）→ **停止**してユーザー判断を仰ぐ（未コミット変更・ローカル独自 commit を破棄しないため。「共通サブ手順: origin への同期」の status 解釈を参照）
   - `evacuated: true` → ユーザーに1行通知: 「メインリポジトリを default branch に退避しました（worktree 作成のため）」
   - PR 番号の解決失敗等は非ゼロ exit + stderr で返る → stderr を提示して停止し、`<pr-number>` の明示指定を促す

2. **`action: "enter_existing"`** → 既存 worktree へ切替のみ:
   - メインセッション: `EnterWorktree(path: <worktree_path>)`
   - 対象リポジトリ外 cwd のサブエージェント: 共通規約に従い Bash `cd` で代替

3. **`action: "create"`** → 作成と切替（作成プリミティブは実行文脈で分岐）:
   - **メインセッション**: `EnterWorktree(name: <worktree_name>)` で作成・切替し、worktree 内で仕上げを実行:
     ```bash
     bash ~/.claude/skills/worktree-resolution/scripts/resolve-pr-worktree.sh finalize <worktree_name> <head_ref>
     ```
     （PR branch への switch・origin 同期・temp branch `worktree-<worktree_name>` の削除。`status: ok` 以外は停止、`warnings[]` は報告に併記）
   - **サブエージェント**（`EnterWorktree(name:)` 不可）:
     ```bash
     bash ~/.claude/skills/worktree-resolution/scripts/resolve-pr-worktree.sh create-fallback <worktree_name> <head_ref>
     ```
     返った `worktree_path` へ切替: 対象リポジトリ内 cwd なら `EnterWorktree(path:)`、対象リポジトリ外 cwd なら Bash `cd` 代替。`status: diverged`（古い同名ローカル branch に独自 commit が残っている残骸）は停止。この経路では `.worktreeinclude` コピー・終了時の自動クリーンアップ判定は働かず、回収は `/cleanup-merged` に委ねる

4. **作業ディレクトリ確認**: `git rev-parse --show-toplevel` が worktree パスを返すことを確認する（Bash `cd` 代替では `cd <worktree-path> && git rev-parse --show-toplevel` に読み替える）

## 共通サブ手順: origin への同期

ローカル branch を `origin/<branch>` の最新に揃える処理。resolve / create-fallback / finalize が内部で実行するため単独の呼び出しは不要で、ここでは status の解釈だけを定める:

- 安全な fast-forward（behind のみ・clean。untracked のみの変更は無視）だけが自動実行され、`synced: true` で返る
- `behind_dirty`: 未コミット変更あり → 停止し、コミット/stash をユーザーに促す（未コミット変更を破棄しないため）
- `diverged`: ローカルに origin に無い commit あり → 停止し、rebase・退避等の手動整理をユーザーに促す（未 push の作業を破棄しないため）

## 共通サブ手順: PR head との鮮度確認

ローカル HEAD が PR の最新 head と整合しているかを確認し、安全なら自動同期する手順。PR を対象にレビュー・修正を行うスキル（/deep-review・/review-response）が差分取得・修正適用の前に実行する:

```bash
bash ~/.claude/skills/worktree-resolution/scripts/check-pr-freshness.sh <pr-context.jsonのパス>
```

引数は fetch-pr-context.sh の出力ファイルパス（`pr.head_oid` / `pr.head_ref` / `pr.base_ref` / `is_own_pr` を読む）。base・head branch の fetch も内部で実行するため事前 fetch は不要。status 別の対応:

- `ok` / `synced` / `ahead_own`（自分の PR に未 push のローカル commit があるだけ） → 続行
- `behind_dirty` / `diverged` → **停止**（「共通サブ手順: origin への同期」と同じ解釈でユーザーに対応を促す）
- `branch_mismatch`（detached HEAD 含む） → **停止**し、`git switch <head_ref>` または `--worktree` での再実行を提示（PR head branch 以外のカレント branch — main 等 — を誤って同期・レビューしないための前提ガード）
- `fetch_failed` → fork 由来 PR（head branch が origin に存在しない — 本手順の対象外）の可能性が高い。エラーとして停止しユーザーに知らせる

## 注意事項（PR worktree 解決時の挙動）

- 並列で別の作業中に呼び出すと、session が PR の worktree に切り替わる。元の作業に戻るには別途 `EnterWorktree(path: <元のworktree>)` を呼ぶ
- 別ターミナル/別 tmux ペインで実行する運用なら、元 session は触らずに済む（並列作業の推奨運用）
- worktree を新規作成する場合、PR の head branch を fetch して checkout するため、PR ブランチ側に未 push のローカル commit があれば事前に push しておくこと
- 既存 worktree を再利用する場合も `origin/<pr-branch>` の最新 head に同期される。worktree に未コミット変更や未 push のローカル commit が残っていると停止するため、事前にコミット/push（または破棄）しておくこと
- **メインリポジトリが PR head branch を checkout 中の場合**: resolve が自動で default branch へ退避する（dirty tree なら `evacuation_dirty` で停止するため、事前にコミット/stash しておくこと）
- **fork からの PR は対象外**: 本手順は head branch が origin に存在する PR のみを対象とする（`git fetch origin <pr-branch>` や `origin/<pr-branch>` 参照が fork PR では成立しないため）。fork PR で fetch が失敗した場合は手順のバグではなく前提外
