---
name: issue-handle
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number | --file FILE_PATH> [--base BRANCH] [--worktree] [--delegate-impl] [--no-plan-review]
disable-model-invocation: true
---

# /issue-handle

## 使用方法
```
/issue-handle 99                              # Issue番号、現在ブランチをベース
/issue-handle --file spec.md                  # ファイル
/issue-handle 99 --worktree                   # Issue番号 + worktree で隔離（並列開発時）
/issue-handle --file spec.md --worktree       # ファイル + worktree
/issue-handle 99 --base develop --worktree    # ベースブランチを明示指定
/issue-handle 99 --worktree --delegate-impl   # 実装ループを Sonnet サブエージェントに委譲
/issue-handle 99 --no-plan-review             # 軽微な Issue 向けに計画検証（deep-plan-review）を省略
```

## Issue情報（自動取得）
!`gh issue view $0 --json title,body,labels,assignees,comments 2>/dev/null || echo "Issue情報の取得をスキップ（--file指定時）"`

## 引数
- `<issue-number>`: 対応するIssue番号（`--file`と排他）
- `--file FILE_PATH`: 仕様ファイルのパス（`<issue-number>`と排他）
- `--base BRANCH`: ベースブランチを明示指定。省略時は起動時の現在ブランチ
- `--worktree`: 実装作業を専用の git worktree で隔離（並列開発時に推奨）。ネイティブの `EnterWorktree` インフラに delegate するため、`WorktreeCreate` hook・`.worktreeinclude`・session 終了時のクリーンアップ判定（変更なし→自動削除、変更あり→保持 or 削除の確認）が機能する
- `--delegate-impl`: 実装フェーズの Step 3〜4（実装・テスト・コミットのループ）と Step 7-2 の決定済み修正の適用を `model: "sonnet"` の実装エージェントに委譲する。判断（計画・レビュー指摘の要否・修正方針）と外向き操作（Issue コメント・PR 作成・PR 説明更新）は親セッションに残る（レビュー修正フェーズの push のみ実装エージェントが行う）
- `--no-plan-review`: 計画フェーズの計画検証（deep-plan-review）をスキップする。ドキュメント・spec の小修正や単一ファイルの軽微な変更など、計画に blocker の出る余地がほぼなく検証コストが見合わない Issue 向け。完了時の独立セッション `/deep-review` は省略されず、安全網として残る

## 前提条件
- Gitリポジトリ内で実行すること
- Issue番号指定時: `gh` CLIがインストール・認証済みであること
- **ベースブランチ**: `--base BRANCH` で明示指定 or 省略時は起動時の現在ブランチ
- `--worktree` 指定時: dirty な working tree のまま現在ブランチ != ベース だと内部の `git switch` が失敗して abort されるため、事前にコミット/stash しておくこと（`worktree.baseRef: "head"` 設定下では HEAD = ベースブランチを最新化した状態から worktree を作るため、必要に応じて事前準備で `git switch` する）

## 実行内容

### 要件確認・調査
- Issue番号指定時: Issue本文・コメントを確認（起動時に取得済み。再取得する場合は `gh issue view <issue-number> --comments`）
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### 計画フェーズ

#### 事前準備（Planモード移行前、Bashで実行）

PlanモードではBashが使えないため、以下を**移行前に**必ず実行する。`--worktree` 指定時は Step 0-9 すべて、非 `--worktree` 時は Step 0/1/3/9 のみ実行（Step 2/4-8 はスキップ）。

**流れの要約**（`--worktree` 新規シナリオ）: 調査 (0) → base 確定・状態記録・fetch (1-3) → 既存 worktree 検出 (4、あれば再開へ) → 名前確定・base 前進・EnterWorktree・メインツリー復元 (5-8) → Plan モード (9)。目玉は Step 8 でメインツリーを起動時状態に戻すことで、Plan モード中も並列作業可能。

**Step 0. 要件確認・調査（最小限）**
- Issue 本文とコメント（`!gh issue view` で取得済み）を読み、続く Step 5 の worktree 名（type + description）判断に必要な範囲で関連コードを Read/Grep
  - コメントは時系列で読み、要件に影響する確定事項（スコープ調整・方針変更・仕様追記）は本文と同格の要件として扱う
  - Bot コメントと minimized なコメント（`isMinimized: true`）は読み飛ばす
- **深追い禁止**: 実装方針の詳細検討・計画起案は Plan モード内で実施（Plan モード内でも Read/Grep は可能、Bash のみ不可）
- 参考: 上記「### 要件確認・調査」セクションは Plan モード内での追加調査時にも用いる共通の指針

**Step 1. ベースブランチの確定**
- `--base BRANCH` 指定時: その値を使用
- 省略時: `git branch --show-current` の値を使用
- 確定した値は計画ファイル記録用に控える

**Step 2. 起動時状態記録**（`--worktree` 指定時のみ）
- 起動時ブランチ: Step 1 で `--base` 省略時は取得済みの値を再利用。`--base` 明示指定時のみ改めて `git branch --show-current` で取得
  - 空（detached HEAD の場合）→ `git rev-parse HEAD` で SHA を代替記録し、`detached` フラグを立てる
- **メインツリー絶対パス**: `git rev-parse --show-toplevel`
  - Step 7 で EnterWorktree による session 切替後は取得できないため、事前に必ず記録する
- **detached HEAD 時のコマンド書き換え規則**: 以降 Step 7/8 の復元コマンド `git switch <起動時ブランチ>` は、detached フラグが立っている場合は `git switch --detach <SHA>` に置換する（各 Step では単に「起動時ブランチへ復元」と記述）

**Step 3. リモート最新化**: `git fetch origin <base-branch>` を常に実行
- 失敗時（リモート未設定等）は警告のみで続行

**Step 4. 既存 worktree 検出と再開判定**（`--worktree` 指定 & Issue番号指定時のみ）
- `git worktree list --porcelain` を実行し、`branch refs/heads/worktree-*-<issue-num>-*` に該当する worktree を検索（`<issue-num>` は引数で受け取った Issue 番号）
- 該当する worktree が見つからない → 新規シナリオ。Step 5 へ進む
- 該当する worktree が見つかった → **再開シナリオ**。以下を実施:
  - `EnterWorktree(path: <found-path>)` で session を切替
  - **origin への同期**: `git fetch origin <branch>` の上で `git merge --ff-only origin/<branch>` を実行する（`<branch>` は検出した worktree の branch = `worktree-...` 形式。別マシン・GitHub UI での suggestion コミット等による push があると、同期なしでは stale な HEAD 基準で再開計画を組んでしまうため。ahead — 未 push のローカル commit のみ — は no-op 成功する）
    - fetch が「リモートに branch が存在しない」理由で失敗 → 同期対象なしとして続行し、報告の origin 項目を「リモート branch なし」とする（未 push、またはマージ済みでリモート branch 削除済みのケース）
    - fetch がその他の理由（ネットワーク・認証等）で失敗 → 警告のみで続行し、報告の origin 項目を「同期未確認」とする（Step 3 の fetch 失敗時と同じ方針）
    - merge が失敗（diverge・dirty との衝突）→ 警告のみで続行し、報告の origin 項目を「乖離あり」とする（未 push のローカル作業を尊重しつつ、扱いは Plan モードでユーザーが判断できる。Step 6 の ff 失敗時と同じ方針）
    - 注: 「共通サブ手順: origin への同期」（worktree-resolution）を使わないのは意図的 — 同手順は ahead を停止条件とするが、再開では未 push のローカル commit が正常状態のため
  - 切替後、Bashで以下を取得して計画起案の前提に組み込む:
    - `git log <base-branch>..HEAD --oneline` で既存コミットの進捗
    - `git status` で未コミット変更
    - worktree 内の前回計画ファイル（探索場所はプロジェクトの慣習に従う。Plan モードで指定される今回の計画ファイルパスとは別物の可能性があるため、見つかれば Read で読み込んでおく）
  - 検出結果はユーザーに 1 行で報告: 「既存 worktree を検出しました（path: ..., 既存コミット N 件、未コミット変更: あり/なし、origin: 同期済み/リモート branch なし/乖離あり/同期未確認）。再開計画として進めます」
  - **Step 5-8 をスキップして Step 9（EnterPlanMode）へ**
- 補足: `--file` 指定時（Issue 番号なし）は worktree 名の予測が安定しないため、本ステップはスキップする。新規シナリオとして進み、実装フェーズの作業ブランチ確定ステップでの衝突検出フォールバックでカバーする

**Step 5. worktree 名確定**（`--worktree` 指定 & 新規シナリオのみ）
- Step 0 の調査結果と Issue 本文から type + description を判断
- フォーマット:
  - Issue 番号あり: `<type>/<issue-number>-<description>`
  - Issue 番号なし（`--file` 指定時）: `<type>/<description>`
- type: feature / fix / hotfix / refactor / chore / docs
- description ルール（モダン standard 準拠）:
  - kebab-case（lowercase + ハイフン区切り）
  - feature 系は動詞から始める（`add-`, `update-`, `remove-`, `refactor-` 等）
  - fix 系は対象を示す名詞句（`null-pointer`, `race-condition` 等）
  - 全体で60文字以内目安
- 例: `feature/99-add-oauth-login`, `fix/42-null-pointer`, `feature/add-login-validation`（--file 指定時）
- **worktree 名は branch 名から `/` を `-` に置換した sanitized 形式**（例: `feature/99-add-oauth-login` → `feature-99-add-oauth-login`。規約と実装挙動注記: @~/.claude/skills/worktree-resolution/SKILL.md の「共通規約」）

**Step 6. ローカル base 前進**（`--worktree` 指定 & 新規シナリオのみ）
- 目的: `worktree.baseRef: "head"` 設定下で EnterWorktree がローカル HEAD を起点に worktree を作るため、HEAD を `origin/<base-branch>` まで前進させる
- 現在ブランチ == ベース → `git merge --ff-only origin/<base-branch>`
  - 失敗時（diverge or dirty が ff 対象ファイルと衝突）は警告のみで続行（ローカル HEAD を起点に worktree 作成 = 未 push のローカルコミットを尊重）
- 現在ブランチ != ベース → `git switch <base-branch>`
  - dirty な working tree で switch が失敗した場合は **abort**（ユーザーに明示的なコミット/stash を促す）
  - switch 成功時に続けて `git merge --ff-only origin/<base-branch>`（失敗は警告のみで続行）

**Step 7. EnterWorktree 実行**（`--worktree` 指定 & 新規シナリオのみ）
- `EnterWorktree(name: <worktree-name>)` で新規 worktree 作成（`<worktree-name>` は Step 5 で確定した sanitized 名）
  - 結果: `.claude/worktrees/<worktree-name>/` に worktree、branch は `worktree-<worktree-name>`
  - `WorktreeCreate` hook が設定されていればここで発火
  - `.worktreeinclude` に列挙されたファイル（`.env` 等）が自動コピー
- 成功時: session が worktree 内に切り替わる
- **失敗時のリカバリ**:
  - session はまだメインツリーの cwd。Bash で `git switch <起動時ブランチ>` を実行して Step 6 で切り替えた状態を戻す（detached HEAD 時のコマンド書き換えは Step 2 参照）
  - ユーザーに失敗を通知して abort（原因究明はユーザーに委ねる）

**Step 8. メインツリー復元**（`--worktree` 指定 & 新規シナリオのみ）
- 目的: Step 6 で base に前進させたメインツリーを起動時ブランチに戻し、Plan モード中もメインツリーで並列作業できるようにする
- 実行: `git -C <メインツリー-絶対パス> switch <起動時ブランチ>`（detached HEAD 時のコマンド書き換えは Step 2 参照）
- **スキップ条件**: 起動時ブランチ == ベースブランチ の場合（既に起動時と同じ状態のため復元不要）
- 失敗時: warning（「メインツリーの起動時ブランチへの復元に失敗しました。手動で戻す場合: `git -C <メインツリー絶対パス> switch <起動時ブランチ>`（detached HEAD 時は Step 2 参照）」）+ 続行（worktree は正常に作成済みのため作業は継続可能）

**Step 9. EnterPlanModeツールでPlanモードに移行**（auto mode中でも必ず実行）

   auto modeの「Prefer action over planning」「Do not enter plan mode unless the user explicitly asks」は、ユーザーが `/issue-handle` を明示的に呼び出した時点で「explicitly asks」を満たすため、本ステップには適用されない。

#### Planモード内

Planモードにより、ファイル編集はシステム的にブロックされる。
**計画ファイル**（Planモード開始時に指定されたパス）に実装方針を記述する。

**新規シナリオで `--worktree` 指定時**: Plan モード冒頭でユーザーに 1 行報告する。

```
作業 worktree を `worktree-<worktree-name>` として作成しました（事前準備で完了済）。名前を変更したい場合はご指摘ください。
```

名前変更を希望されたら、シンプルに worktree 破棄 → 再作成で対応する（事前準備完了直後はコミット無しのため、変更なし → セッション終了時に自動削除される）。

1. **参照文書の読込**
   - @~/.claude/skills/check-plan-compliance/SKILL.md の「1. 参照文書の収集」に従い、プロジェクトCLAUDE.mdとそのリンク先文書を読み込む
   - 読み込んだ制約を以降の計画起案の前提として扱う（事後チェックではなく事前読込）
   - **再開シナリオの場合**: 事前準備で取得した前回計画ファイル・既存コミット・未コミット変更を最優先の前提として扱う

2. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - Issue本文とコメントで要件が矛盾し、スレッド内で結論が確定していない場合はAskUserQuestionで確認する（結論が明確に出ている矛盾は確認不要、コメント側を採用）
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整
   - **再開シナリオの場合**: 前回計画から完了済み項目を洗い出し、**残タスクのみ**を今回の計画として組み立てる（前回方針を踏襲、必要に応じて再調整）

3. **タスク作成**
   - 方針確定後、TaskCreateツールで実装フェーズの各タスクを作成
   - 各タスクには `subject`（命令形）と `activeForm`（進行形）を設定
   - 例: subject「テストを実装」、activeForm「テストを実装中」

4. **計画完了**
   - **計画ファイルに以下を含める**:
     - ブランチ名（typeを含む完全な形式）
     - ベースブランチ（取得済みの値）
     - Issue番号（Issue番号指定時。`gh issue comment` や `Closes #N` で使用）
     - worktree 使用（`--worktree` 指定時 true）
     - 実装委譲（`--delegate-impl` 指定時 true。実装フェーズと Step 7-2 の分岐判定に使う。再開シナリオでも今回の起動引数を正とし、前回計画の値は引き継がない）
     - worktree 名（`--worktree` 指定時のみ。ブランチ名から `/` を `-` に置換した sanitized 名、例: `feature-99-add-oauth`）
       - 注: `--worktree` 指定時の実 branch 名は `worktree-<worktree名>` 形式になる（Claude Code 仕様。PR の head branch もこの形式）
     - worktree 作成状態（`--worktree` 指定時のみ）: 事前準備で完了済（新規シナリオ）／既存 worktree に切替済（再開シナリオ）
     - 起動時ブランチ（`--worktree` & 新規シナリオ時のみ。Step 2 で記録した値）
       - detached HEAD 起動時は SHA と `detached` フラグを併記
     - メインツリー絶対パス（`--worktree` & 新規シナリオ時のみ。Step 2 で記録した値）
     - 上記2項目（起動時ブランチ・メインツリー絶対パス）の用途: Plan モードで承認前に Esc 中断された場合や Step 8 が失敗した場合に、手動でメインツリーの状態を確認・復元するためのメタデータ
     - 言語方針（`git log` / `gh pr list --limit 5` で事前確認）:
       - コミット: 日本語 / 英語
       - PR: 日本語 / 英語
       - （慣例が混在する場合のみ）判断根拠を1行で明記
     - 想定コミット計画（複数コミットになる場合のみ記述）:
       - 例:
         - コミット1: <内容>
         - コミット2: <内容>
       - 同じファイルに無関係な変更が混ざるのを防ぎ、各段階でテストを通せる単位に分ける
       - 実装中の調整は許容（厳密に固定しない）
     - シナリオ種別（事前準備の検出結果に基づく。`新規` / `再開`）
     - 実装手順チェックリスト:
       - [ ] 実装方針をIssueにコメント（Issue番号指定時のみ。取得済みコメントに自分の実装方針コメント — `viewerDidAuthor: true` かつ `## 実装方針` 見出し — があればスキップ。ただし今回の計画が投稿済み方針から実質的に変わる場合は更新版を投稿）
       - [ ] 作業ブランチ作成（`--worktree` 指定時は**事前準備で完了済のため本項目全体をスキップ**。非 `--worktree` 時のみ実装フェーズで実施）
       - [ ] 実装・テスト（想定コミット計画の単位で都度コミット、必要に応じて調整）
       - [ ] Test, Lint成功確認
       - [ ] `/simplify` で品質チェック・修正
       - [ ] プッシュ・PR作成（Issue番号指定時は `Closes #<issue-number>` を含める）
       - [ ] 独立セッションでの `/deep-review` 実行（サブエージェント経由）→ 親で自動修正
   - **計画準拠チェック**: @~/.claude/skills/check-plan-compliance/SKILL.md の Step 2〜4 を実行（Step 1 は本Planモード冒頭で実施済みのためスキップ。`--no-plan-review` 未指定時は後続の計画検証が ExitPlanMode を担うため、Step 4 の ExitPlanMode は呼ばず計画修正までに留める。`--no-plan-review` 指定時は後続の計画検証が無いため、同スキルの原則どおり Step 4 の ExitPlanMode まで実行する）
   - **計画検証**（`--no-plan-review` 未指定時のみ）: @~/.claude/skills/deep-plan-review/SKILL.md を実行（修正後の計画での ExitPlanMode まで同スキルが担うため、本スキル側で重複して呼ばない）
   - ユーザーの承認を待つ

5. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### 実装フェーズ

1. **実装方針をIssueにコメント**（Issue番号指定時のみ）
   - 以下のフォーマットで投稿:
     ```
     ## 実装方針

     ### 概要
     （何を実装するかの1-2文の説明）

     ### 主な変更点
     - （変更点1）
     - （変更点2）

     ### 影響範囲
     - （影響するファイル/コンポーネント）

     ### 対応ブランチ
     `<ブランチ名>`
     ```
   - `gh issue comment` でIssueに投稿

2. **作業ブランチ確定**
   - **`--worktree` 指定時**: 計画フェーズ事前準備で worktree 作成・session 切替は完了済（新規/再開シナリオともに）。本ステップ全体をスキップして次のステップ（実装・テスト修正）へ
   - **非 `--worktree` 時のみ以下を実施**:
     - ブランチ命名は事前準備 Step 5「worktree 名確定」の規約に従う（type enum、description ルール、フォーマット、`--file` 指定時の分岐すべて `--worktree` 有無に関わらず共通）
     - 分岐元: 計画ファイルに記録したベースブランチを明示する
       - 例: `git switch -c feature/99-xxx origin/<base-branch>`（事前準備の fetch 成功時）
       - 例: `git switch -c feature/99-xxx <base-branch>`（fetch 失敗時のフォールバック）
     - ベースブランチのリモート最新化は計画フェーズ事前準備（Step 3）で完了済み

#### 実装委譲（`--delegate-impl` 指定時のみ）

Step 3〜4 を実装エージェントに委譲する。本ブロック完了後は Step 3〜4 をスキップして Step 5（品質チェック）へ進む。

**起動**
- Agent ツールで `subagent_type: "claude"`・`model: "sonnet"` のサブエージェント（実装エージェント）を起動する（承認済み計画に沿った実装は判断密度が低く出力量が多いため下位モデルを充てる）
  - `fork` は model 指定が無視され常に親モデルを継承するため使わない
- 起動結果の agent ID を控える（以降の再開・照会・修正指示はすべて SendMessage で同一エージェントに送るため。agent ID はセッションをまたいで有効ではない）
- 実装エージェントは親と同じ session cwd（`--worktree` 時は worktree 内）で動くため、worktree・ブランチ操作の指示は不要

**プロンプトに含める契約**
- このセッションが実装専用であり、計画起案時の会話コンテキストを持たない旨
- 計画ファイルの絶対パス（Read して承認済み計画に従う）
- 作業範囲: 実装・テスト（想定コミット計画の単位で都度コミット）、Test/Lint 成功確認（失敗時は修正 → コミット → 再テスト）
  - コミットは @~/.claude/skills/git-commit/SKILL.md に従い、言語は計画の言語方針に従う（自動言語判定はスキップ）
  - テスト完了待ち等で後続単位の編集へ先行する場合も、編集を終えた計画単位は `git add <パス指定>` で先にコミットしてから進める（複数単位の未コミット変更を working tree に溜めない。実行中のテストが読むファイルと編集対象が重なる場合は先行編集しない）。計画が「テスト成功確認後にコミット」を指定していても、テスト待ちが発生する場合は先行コミットを優先し、テストで問題が出たら修正コミットを追加する（git-commit の自己完結原則の検証をテスト完了後へずらす意図的なトレードオフ）
- **長時間コマンドの待ち方**: 完了待ちだけを目的にターンを終えない（サブエージェントのターン終了は親への報告として返り、再開に SendMessage の往復が必要になる。`TaskOutput` 相当のブロッキング取得ツールは無い前提で振る舞う）
  - 余裕を持って 10 分以内に終わる見込み → フォアグラウンド（`timeout` 上限の 600000ms まで指定可）で同一ターン内に完了させる
  - それ以外（上限に近い・超える見込み）→ バックグラウンド起動し、後続の実作業（次の計画単位の編集等）を進めた上で、ターンを終える前にプロセスの進行（CPU 時間・ログ更新）を確認する
  - 進行していないと確認できた場合 → 完了通知を待たず切り分けへ進み、解決できなければ停止条件のエスカレーション報告で停止する（起動直後で進行の判断材料が無い場合は通知待ちでよい）
- **既知のテスト失敗パターン**: ローカル環境依存で失敗・長時間化する既知事象（外部サービス未起動・DB スキーマの古さ・flaky 等）が親の知識（プロジェクト CLAUDE.md・memory・計画時の調査）にあれば handoff に含め、テスト失敗時の切り分け基準を与える（既知パターンに合致し本変更と無関係 → 実装報告に記録して続行 / 判断がつかない → 停止条件 (c) の 3 回試行を待たず、同じ報告フォーマットでエスカレーション）
- **禁止事項**: push・PR 作成・/simplify・Issue コメントは行わない（親が実施。push は後続のレビュー修正フェーズで親が SendMessage で明示指示した場合のみ）
- **停止条件（blocker 乖離）**: 以下のいずれかに該当したら実装を続けず、「何が乖離したか・選択肢・推奨」を構造化したエスカレーション報告を返して停止する（エージェントは破棄されず、親の SendMessage で再開できる）
  - (a) 計画の前提が実コードと食い違い、アプローチ自体の変更が必要
  - (b) スコープの変更・縮小が必要（仕様遵守原則がユーザー確認を要求する領域）
  - (c) テスト失敗が計画のアプローチ内で 3 回試して解決しない
- **minor 乖離**（計画が指定しない実装詳細・コミット境界の微調整）は裁量で続行し、実装報告に記録する。ただし計画が明示するコミット構成の統合・分割は minor ではなく、維持できない場合はエスカレーションする（テスト失敗起因の修正コミットの追加は分割に当たらず、minor として記録する）
- **完了時の実装報告**（diff から読み取れない情報のみに絞る）:
  - 計画からの乖離点とその理由
  - 非自明な実装判断とその理由
  - 試して捨てたアプローチ
  - テスト・Lint の実行結果

**親側のエスカレーション対応**
- 事実の不足 → 親が調査して決定 / 判断の不足 → AskUserQuestion でユーザーに確認
- 決定を SendMessage で同一エージェントに送って再開させる
- 完了報告・エスカレーション報告以外の中間報告（完了待ちのままターンが終わった場合）→ 即座に SendMessage で再開させず、バックグラウンドタスクの完了通知による自動再開を待つ（長時間進展が無い場合のみ状況確認を送る）
- サブエージェント失敗時（Agent ツールが null/error を返した場合）の扱いは 7-2 冒頭と同じ（自動リトライしない）

**完了時の親の確認**
- 実装報告を受領後、`git log --oneline` と `git diff --stat` で計画どおりのコミット構成かを確認する（diff 内容の正当性確認は Step 5 の /simplify と Step 7 のレビューが担うため、ここでは全文通読しない）。計画外の修正コミットがある場合は実装報告の記録（テスト失敗起因か）と突合する

3. **実装・テスト修正**
   - 実装とテストの順序は柔軟に対応
   - **想定コミット計画の単位で都度コミット**（最後にまとめてではなく）
     - 計画した単位ごとに、実装 → テスト確認 → コミット のサイクルを回す
     - 同じファイルに無関係な変更が混ざる前にコミットすることで、後からの hunk 分割を回避
     - 実装中に計画と現実が乖離した場合は、コミット境界を調整してよい（計画通りの固定にこだわらない）
   - コミット方針は git-commit.md に従う
   - コミット・PRの言語: 計画で確定した方針に従う（git-commit / git-pr の自動言語判定はスキップ）

4. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行
   - 数分以上かかる見込みの場合は `run_in_background: true` で実行（sleep ポーリングを避けキャッシュを節約）
   - **失敗した場合**: 修正 → コミット → 再テストを繰り返す
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

5. **品質チェック**
   - `/simplify` を実行し、変更コードの再利用性・品質・効率性を確認・修正
   - 修正があればコミット

6. **実装完了処理**
   - 未コミットの変更があれば @~/.claude/skills/git-commit/SKILL.md に従ってコミット
   - @~/.claude/skills/git-pr/SKILL.md に従ってプッシュ・PR作成
     - 計画ファイルに記録したベースブランチを `/git-pr --base <base-branch>` として引き渡す
   - PR説明にIssue/仕様の背景・動機を含める（リンクだけでなく「なぜこの変更が必要か」を本文に書く）
   - Issue番号指定時: `Closes #<issue-number>` を含める

7. **独立セッションでのレビュー → 親での自動修正**
   - **目的（関心の分離）**:
     - **レビュー（発見）**: 実装バイアスを排除するため独立セッションで実施
     - **判断・修正**: 誤指摘・前提誤りを判別するため、実装コンテキストを持つ親セッションで実施
       - `--delegate-impl` 時、親は計画 + 実装報告 + diff を判断材料とし、実装経緯が必要で報告に無い場合は SendMessage で実装エージェントに照会して補う

   7-1. **サブエージェントでレビュー実行**
   - Agent ツールで `subagent_type: "claude"`・`model: "fable"` のサブエージェントを起動する（レビューは入力が有界で判断密度が高いため上位モデルを充てる）
     - `fork` は親コンテキストを継承するため使わない（実装バイアスが残るため目的に反する。また `fork` は model 指定が無視され常に親モデルを継承する）
   - サブエージェントへのプロンプトに以下を含める:
     - このセッションが独立レビュー専用であり、親セッションの実装コンテキストを持たない旨
     - 実行コマンド: `/deep-review <pr-number> --issue <issue-number> --no-autofix`
       - `<pr-number>`: ステップ6で作成/更新した PR 番号（`gh pr view --json number -q .number` で取得）
       - `<issue-number>`: Issue 番号（`--file` 指定時は `--issue <issue-number>` 部分を省略）
       - `--no-autofix`: 自動修正を強制OFF（修正は親セッションで行うため）
       - ベースブランチは deep-review 側で自動判定（PR のベースブランチを採用）
       - 補足: サブエージェントは親と同じ worktree (PR の head branch) で動くため `--worktree` は付けない
     - レビュー結果をそのまま返すよう指示（追加の解釈・要約は不要）
     - 補助コンテキスト: 作業ブランチ名、PR URL（既知の場合）

   7-2. **親セッションで自動修正**
   - サブエージェント失敗時（Agent ツールが null/error を返した場合）はエラーを表示してユーザー判断を仰ぐ（自動リトライしない）
   - サブエージェントから返ってきたレビュー結果を親セッションで表示
   - そのレビュー結果を入力として、@~/.claude/skills/deep-review/SKILL.md の「### 自動対応モードON時: レビュー指摘の反映」セクションに従って以下を実行:
     1. **指摘事項の判断**: 必須修正・推奨修正・質問/確認事項それぞれの基準で対応可否を判断
        - 親は実装コンテキストを持つため、誤指摘・前提誤りを正しく弾ける（`--delegate-impl` 時は実装報告を判断材料に加える）
        - テストコード変更時は @~/.claude/context/test-implementation.md 準拠（過剰なモック追加・テストskipでの誤指摘回避は禁止）
     2. **対応リストの出力**: 対応する指摘・対応しない指摘（理由付き）を表示
        - `--delegate-impl` 時は対応する各指摘に修正方針（どう直すか）まで含める（マイクロな実装判断まで実装エージェントに落とさないため）
     3. **対応すべきものがあれば**: working tree に修正適用 → コミット → テスト・Lint → プッシュ・PR更新
        - 非 `--delegate-impl` 時: 親が実施
        - `--delegate-impl` 時: 対応リスト（指摘 + 修正方針）を SendMessage で実装エージェントに送り、修正適用 → コミット → テスト・Lint → プッシュまでを実施させる（実装委譲ブロックの禁止事項のうち push をここで解除する。停止条件は実装委譲ブロックと同じ）。完了後、親が修正コミットの diff を確認して意図どおりか検証し、PR 説明の更新（要否判断を含む）は親が実施する
          - 対応する指摘がすべて軽微・機械的（typo 等の1行修正レベル）な場合は、SendMessage の往復コストを避けて親が直接適用してよい
          - 実装エージェントが失われている場合（SendMessage 失敗・セッション再開後等）は、新しいサブエージェントを起こさず親が自ら適用する
     4. **対応すべきものがゼロなら**: ここで終了

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している（ローカル環境依存の既知失敗と切り分け済みのものは、実装報告への記録をもって充足とする）
- [ ] 変更がプッシュされている
- [ ] PRが作成されている（または既存PRが更新されている）
- [ ] 独立セッションでの `/deep-review` を実施し、結果を親セッションで表示済み
- [ ] レビュー指摘のうち親が「対応する」と判断したものは適用・コミット・プッシュ済み（対応すべきものがゼロなら何もしない）

## 注意事項
- **Planモード中**: ファイル編集・ブランチ作成はシステム的にブロックされる
- **auto mode下での運用**: auto modeであっても計画フェーズ（EnterPlanMode）はスキップしない
- **テスト失敗時**: 修正 → コミット → 再テストのサイクルを繰り返す
- **`--worktree` 指定時の前提・挙動**:
  - `worktree.baseRef: "head"` 設定が前提（定義: @~/.claude/skills/worktree-resolution/SKILL.md の「共通規約」）
  - 並列で複数 issue を進める場合、issue 1 つにつき 1 つの Claude session（別ターミナル/別 tmux ペイン）が必要
  - Plan モード中もメインツリーは起動時ブランチに復元されており並列で別作業可能（メインツリー占有は事前準備完了までの数秒）
  - **branch 名は `worktree-<sanitized>` 形式になる**（Claude Code 仕様。`worktree-` プレフィックスは剥がさない）。PR の head branch もこの形式
  - DB 等の永続リソースを使うプロジェクトでは、各プロジェクト個別に `WorktreeCreate` / `WorktreeRemove` hook を設定して per-worktree に DB を分離する（スキル本体は DB を意識しない）
  - `.env` 等の gitignored ファイルは各プロジェクト個別に `.worktreeinclude` で列挙
  - クリーンアップ: session 終了時にネイティブ機能が自動判定（変更なし→自動削除、変更あり→確認プロンプト）。手動で片付ける場合は `git worktree remove <path>` + `git branch -d <branch>`
- **`--delegate-impl` の前提と限界**: deep-plan-review が保証する計画の自己完結性が handoff 品質の前提となる。deep-review の指摘数やエスカレーションが目立って増える場合は、委譲をやめる前に計画側の粒度（blocker 基準を「Sonnet が迷わない」水準へ締める）を疑う
- **`--no-plan-review` と `--delegate-impl` の併用は非推奨**: 上記のとおり計画の自己完結性検証が handoff 品質の前提のため、未検証の計画を Sonnet へ委譲すると品質低下リスクが上がる。両方が指定された場合はその旨を警告した上で続行する（停止はしない）
