---
name: worktree-resolution
description: PR・ブランチに対応する git worktree の解決手順（既存検索・切替・新規作成）と命名規約、およびレビュー系スキル共有の PR head 鮮度確認サブ手順
user-invocable: false
---

# worktree-resolution

PR・ブランチに対応する worktree を解決（既存検索・切替・新規作成）するための共通規約と手順。`/issue-handle`・`/review-response`・`/deep-review` の `--worktree` から参照される。PR を対象にレビュー・修正を行うスキルが共有する鮮度確認サブ手順もここに置く（こちらは worktree の有無に関わらず参照される）。

## 共通規約

worktree を扱う全スキルが従う契約。乖離するとスキル間で worktree を相互発見できなくなる。

- **worktree 名の計算**: branch 名から `/` を `-` に置換する
  - 例: `feature/99-add-oauth` → `feature-99-add-oauth`
  - **必ず sanitize してから `EnterWorktree(name:)` に渡す**。`/` を含む値を渡すと Claude Code 実装により `+` に置換され、以降の既存 worktree 検索パターンから外れて再開検出に失敗する（2026-07-06 実測、公式未文書挙動）
- **既存 worktree の検索**: `git worktree list --porcelain` を解析し、`branch refs/heads/<branch>` が登録されている worktree を探す
- **EnterWorktree の使い分け**: 既存 worktree への切替は `EnterWorktree(path: <found-path>)`、新規作成は `EnterWorktree(name: <worktree-name>)`
  - **サブエージェント内では新規作成（`name:`）は一律拒否される**（"EnterWorktree cannot create a worktree from a subagent with a cwd override" エラー。明示的な `isolation`・`cwd` 指定がないサブエージェントでも発生する — 2026-07-07 実測。公式仕様上サブエージェントは `path` 形式のみ・対象は `.claude/worktrees/` 配下限定）。PR worktree 解決時の代替手順は手順 5-B 内の分岐を参照
  - **`path:` 切替もエージェント起動時に固定されたプロセス cwd で「現在のリポジトリ」を判定する**（Bash の `cd` では変わらない）。対象リポジトリ外の cwd で起動されたサブエージェント（/review-assigned-prs の clone dir 構成等）では `path:` 切替も "the current directory is not in a git repository" で失敗する（2026-07-07 実測）。該当する場合は EnterWorktree を試行せず、各 Bash 呼び出しに `cd <対象パス> && ...` を前置して作業する（worktree 作成前のリポジトリ操作は対象リポジトリへ、作成後の作業は worktree へ。git 操作は `git -C <対象パス>` でも可、ファイル操作は絶対パスを使用）
- **EnterWorktree 後の絶対パス**: session cwd は worktree に切り替わるが、Edit/Write に渡す絶対パスは自動変換されない。切替**前**に Read したメインツリー絶対パスを Edit/Write に流用しない（機械的強制: `~/.claude/hooks/worktree-edit-guard.sh` がブロックする）
- **前提**: `worktree.baseRef: "head"` 設定（`~/.claude/settings.json`、dotfiles では設定済み）

## 共通サブ手順: origin への同期

ローカル branch を `origin/<branch>` の最新に揃える手順。手順 5-A（既存 worktree 再利用時）・5-B（stale なローカル branch 検出時）から参照される。

1. `git fetch origin <branch>` で remote tracking ref を更新
2. `git rev-list --left-right --count <branch>...origin/<branch>` で乖離を確認:
   - 乖離なし → そのまま続行
   - behind のみ（ローカル側に独自 commit なし）→ dirty 検出: `git status --porcelain | grep -v '^??' | head -n1` で modified/staged 変更を確認（untracked のみは無視）。非空なら停止してユーザー判断を仰ぎ（未コミット変更を破棄しないため）、clean なら `git merge --ff-only origin/<branch>` でリモートに揃える（behind のみなので原則成功する。untracked ファイルが同期先の新規 tracked ファイルと衝突した場合はエラー停止し、その場合も作業は破棄されない。「PR head との鮮度確認」と同じ同期方式）
   - ローカル側に独自 commit あり → 警告して停止し、ユーザー判断を仰ぐ（未 push の作業を破棄しないため）

## 共通サブ手順: PR head との鮮度確認

ローカル HEAD が PR の最新 head と整合しているかを確認し、安全なら自動同期する手順。PR を対象にレビュー・修正を行うスキル（/deep-review・/review-response）が差分取得・修正適用の前に実行する。入力は fetch-pr-context.sh の `pr.head_oid`・`is_own_pr`・`pr.head_ref`。

前提: `git fetch origin <head-branch>` 実施済み（`pr.head_oid` のオブジェクトがローカルに存在すること）。fork からの PR（head branch が origin に存在しない）は対象外で、この fetch が失敗した場合は前提外としてエラーで停止しユーザーに知らせる

上から順に評価する:

1. `git rev-parse --abbrev-ref HEAD` が `pr.head_ref` と一致することを確認。不一致（detached HEAD 含む）→ **エラーで停止**し、`git switch <pr.head_ref>` または `--worktree` での再実行を提示（PR head branch 以外のカレント branch — main 等 — を後続の手順で誤って fast-forward しないための前提ガード）
2. `git rev-parse HEAD` が `pr.head_oid` と一致 → 続行
3. 自分の PR（`is_own_pr: true`）かつ `git merge-base --is-ancestor <pr.head_oid> HEAD` が真（ローカルに未 push の commit があるだけ）→ 続行（push 前のローカル作業を対象にする正当なケース）
4. behind のみ（`git merge-base --is-ancestor HEAD <pr.head_oid>` が真）→ dirty 検出: `git status --porcelain | grep -v '^??' | head -n1` で modified/staged 変更を確認（untracked のみは無視）。空なら `git merge --ff-only <pr.head_oid>` で自動同期して続行（fast-forward のみのため作業を破棄するリスクがない）、非空なら停止してコミット/stash を促す（未コミット変更の破棄を避けるため）
5. 上記以外（diverged、他人の PR にローカル独自 commit がある等）→ **エラーで停止**し、手動での同期・整理（独自 commit の rebase・退避等）を促す（stale なコードを対象にすると誤スコープの指摘・修正になり、コメント投稿時は行番号ずれによる 422 も起きる。一方、自動解決はローカル commit の破棄リスクがあるため行わない。branch 自体が乖離した状態のため `--worktree` でも解決しない）

## PR worktree 解決手順

対象 PR の head branch に対応する worktree に session を切り替える手順。

1. **PR 番号確定**
   - `<pr-number>` が指定されていればそれを使用
   - 無ければ `gh pr view --json number -q .number` でカレント branch の PR を推論
     - 推論失敗時（PR が無い・複数該当など）はエラーメッセージを出して停止し、ユーザーに `<pr-number>` の明示指定を促す（誤った PR に対応しないための安全策）

2. **PR の head branch 名取得**
   - `gh pr view <PR> --json headRefName -q .headRefName`

3. **worktree 名の計算**: 共通規約に従い branch 名から worktree 名を計算

4. **既存 worktree の検索**: 共通規約に従い `<pr-branch>` の worktree を検索し、見つかればそのパスを記録

5-A. **既存 worktree あり**:
   - `EnterWorktree(path: <found-path>)` で session を切替（対象リポジトリ外 cwd のサブエージェントでは共通規約に従い試行せず最初から Bash `cd` で代替）
   - 「共通サブ手順: origin への同期」を `<pr-branch>` に対して実行する（worktree 作成後に PR 側へ push されていると、同期なしでは古いコードを対象にしてしまうため）

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
     - **サブエージェント内では拒否されるため代替経路を使う**（共通規約参照。サブエージェント内と分かっている場合は試行せず最初から代替してよい）: `git worktree add --detach "$(dirname "$(git rev-parse --path-format=absolute --git-common-dir)")/.claude/worktrees/<worktree-name>"` で detached worktree を作成し（`--git-common-dir` 起点なのは worktree 内から実行してもメイン worktree ルート配下に作るため。この経路では `WorktreeCreate` hook・`.worktreeinclude` コピー・終了時の自動クリーンアップ判定は働かず、回収は `/cleanup-merged` に委ねる）、`EnterWorktree(path: <作成した絶対パス>)` で切替（対象リポジトリ外 cwd のサブエージェントでは共通規約に従い試行せず Bash `cd` で代替）。以降は次の `git switch <pr-branch>` から通常経路に合流する
   - worktree 内で `git switch <pr-branch>` で PR の実 branch に切替
     - local に `<pr-branch>` が無い場合は git の DWIM 挙動で `origin/<pr-branch>` から自動作成（modern git 2.23+）
     - local に同名の古い `<pr-branch>` が残っている場合（前回作業の残骸等）はそちらに切り替わり、`origin/<pr-branch>` と乖離するリスクがある。「共通サブ手順: origin への同期」を `<pr-branch>` に対して実行してリモートに揃える（fetch は 5-B 内で実施済みのため省略可。作成直後の worktree のため dirty 検出は通常空振りする）
   - `git branch -d worktree-<worktree-name>` で temp branch を削除（代替経路では temp branch が無いためスキップ）
   - 補足: この 2 段階方式（通常経路）により hook 発火を確保しつつ、目的の PR branch に到達できる

6. **作業ディレクトリ確認**: worktree 内にいることを `git rev-parse --show-toplevel` で確認する（Bash `cd` 代替経路では `cd <worktree-path> && git rev-parse --show-toplevel` が worktree パスを返すことの確認に読み替える）

## 注意事項（PR worktree 解決時の挙動）

- 並列で別の作業中に呼び出すと、session が PR の worktree に切り替わる。元の作業に戻るには別途 `EnterWorktree(path: <元のworktree>)` を呼ぶ
- 別ターミナル/別 tmux ペインで実行する運用なら、元 session は触らずに済む（並列作業の推奨運用）
- worktree を新規作成する場合、PR の head branch を fetch して checkout するため、PR ブランチ側に未 push のローカル commit があれば事前に push しておくこと
- 既存 worktree を再利用する場合も `origin/<pr-branch>` の最新 head に同期される。worktree に未コミット変更や未 push のローカル commit が残っていると停止するため、事前にコミット/push（または破棄）しておくこと
- **メインリポジトリが PR head branch を checkout 中の場合**: 手順 5-B により自動で default branch へ退避される。dirty tree の場合は abort されるため、事前にコミット/stash しておくこと
- **fork からの PR は対象外**: 本手順は head branch が origin に存在する PR のみを対象とする（`git fetch origin <pr-branch>` や `origin/<pr-branch>` 参照が fork PR では成立しないため）。fork PR で fetch が失敗した場合は手順のバグではなく前提外
