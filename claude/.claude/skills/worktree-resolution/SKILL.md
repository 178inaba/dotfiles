---
name: worktree-resolution
description: PR・ブランチに対応する git worktree の解決手順（既存検索・切替・新規作成）と命名規約
user-invocable: false
---

# worktree-resolution

PR・ブランチに対応する worktree を解決（既存検索・切替・新規作成）するための共通規約と手順。`/issue-handle`・`/review-response`・`/deep-review` の `--worktree` から参照される。

## 共通規約

worktree を扱う全スキルが従う契約。乖離するとスキル間で worktree を相互発見できなくなる。

- **worktree 名の計算**: branch 名から `/` を `-` に置換する
  - 例: `feature/99-add-oauth` → `feature-99-add-oauth`
  - **必ず sanitize してから `EnterWorktree(name:)` に渡す**。`/` を含む値を渡すと Claude Code 実装により `+` に置換され、以降の既存 worktree 検索パターンから外れて再開検出に失敗する（2026-07-06 実測、公式未文書挙動）
- **既存 worktree の検索**: `git worktree list --porcelain` を解析し、`branch refs/heads/<branch>` が登録されている worktree を探す
- **EnterWorktree の使い分け**: 既存 worktree への切替は `EnterWorktree(path: <found-path>)`、新規作成は `EnterWorktree(name: <worktree-name>)`
- **前提**: `worktree.baseRef: "head"` 設定（`~/.claude/settings.json`、dotfiles では設定済み）

## 共通サブ手順: origin への同期

ローカル branch を `origin/<branch>` の最新に揃える手順。手順 5-A（既存 worktree 再利用時）・5-B（stale なローカル branch 検出時）から参照される。

1. `git fetch origin <branch>` で remote tracking ref を更新
2. `git rev-list --left-right --count <branch>...origin/<branch>` で乖離を確認:
   - 乖離なし → そのまま続行
   - behind のみ（ローカル側に独自 commit なし）→ dirty 検出: `git status --porcelain | grep -v '^??' | head -n1` で modified/staged 変更を確認（untracked のみは無視）。非空なら停止してユーザー判断を仰ぎ（未コミット変更を破棄しないため）、clean なら `git reset --hard origin/<branch>` でリモートに揃える
   - ローカル側に独自 commit あり → 警告して停止し、ユーザー判断を仰ぐ（未 push の作業を破棄しないため）

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
   - `EnterWorktree(path: <found-path>)` で session を切替
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
   - worktree 内で `git switch <pr-branch>` で PR の実 branch に切替
     - local に `<pr-branch>` が無い場合は git の DWIM 挙動で `origin/<pr-branch>` から自動作成（modern git 2.23+）
     - local に同名の古い `<pr-branch>` が残っている場合（前回作業の残骸等）はそちらに切り替わり、`origin/<pr-branch>` と乖離するリスクがある。「共通サブ手順: origin への同期」を `<pr-branch>` に対して実行してリモートに揃える（fetch は 5-B 内で実施済みのため省略可。作成直後の worktree のため dirty 検出は通常空振りする）
   - `git branch -d worktree-<worktree-name>` で temp branch を削除
   - 補足: この 2 段階方式により hook 発火を確保しつつ、目的の PR branch に到達できる

6. **作業ディレクトリ確認**: worktree 内にいることを `git rev-parse --show-toplevel` で確認する

## 注意事項（PR worktree 解決時の挙動）

- 並列で別の作業中に呼び出すと、session が PR の worktree に切り替わる。元の作業に戻るには別途 `EnterWorktree(path: <元のworktree>)` を呼ぶ
- 別ターミナル/別 tmux ペインで実行する運用なら、元 session は触らずに済む（並列作業の推奨運用）
- worktree を新規作成する場合、PR の head branch を fetch して checkout するため、PR ブランチ側に未 push のローカル commit があれば事前に push しておくこと
- 既存 worktree を再利用する場合も `origin/<pr-branch>` の最新 head に同期される。worktree に未コミット変更や未 push のローカル commit が残っていると停止するため、事前にコミット/push（または破棄）しておくこと
- **メインリポジトリが PR head branch を checkout 中の場合**: 手順 5-B により自動で default branch へ退避される。dirty tree の場合は abort されるため、事前にコミット/stash しておくこと
- **fork からの PR は対象外**: 本手順は head branch が origin に存在する PR のみを対象とする（`git fetch origin <pr-branch>` や `origin/<pr-branch>` 参照が fork PR では成立しないため）。fork PR で fetch が失敗した場合は手順のバグではなく前提外
