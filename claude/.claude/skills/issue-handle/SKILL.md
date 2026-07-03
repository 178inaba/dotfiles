---
name: issue-handle
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number | --file FILE_PATH> [--base BRANCH] [--worktree]
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
```

## Issue情報（自動取得）
!`gh issue view $0 --json title,body,labels,assignees,comments 2>/dev/null || echo "Issue情報の取得をスキップ（--file指定時）"`

## 引数
- `<issue-number>`: 対応するIssue番号（`--file`と排他）
- `--file FILE_PATH`: 仕様ファイルのパス（`<issue-number>`と排他）
- `--base BRANCH`: ベースブランチを明示指定。省略時は起動時の現在ブランチ
- `--worktree`: 実装作業を専用の git worktree で隔離（並列開発時に推奨）。ネイティブの `EnterWorktree` インフラに delegate するため、`WorktreeCreate` hook・`.worktreeinclude`・session 終了時のクリーンアップ判定（変更なし→自動削除、変更あり→保持 or 削除の確認）が機能する

## 前提条件
- Gitリポジトリ内で実行すること
- Issue番号指定時: `gh` CLIがインストール・認証済みであること
- **ベースブランチ**: `--base BRANCH` で明示指定 or 省略時は起動時の現在ブランチ
- `--worktree` 指定時: dirty な working tree のまま現在ブランチ != ベース だと内部の `git switch` が失敗して abort されるため、事前にコミット/stash しておくこと（`worktree.baseRef: "head"` 設定下では HEAD = ベースブランチを最新化した状態から worktree を作るため、必要に応じて事前準備で `git switch` する）

## 実行内容

### 要件確認・調査
- Issue番号指定時: `gh issue view <issue-number>` でIssue内容を確認
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### 計画フェーズ

#### 事前準備（Planモード移行前、Bashで実行）

PlanモードではBashが使えないため、以下を**移行前に**必ず実行する。

1. **ベースブランチの確定**
   - `--base BRANCH` 指定時: その値を使用
   - 省略時: `git branch --show-current` の値を使用
   - 確定した値は計画ファイル記録用に控える

2. **リモート最新化**: `git fetch origin <base-branch>` を常に実行
   - 失敗時（リモート未設定等）は警告のみで続行

3. **既存 worktree 検出と再開判定**（`--worktree` 指定 & Issue番号指定時のみ）
   - `git worktree list --porcelain` を実行し、`branch refs/heads/worktree-*-<issue-num>-*` に該当する worktree を検索（`<issue-num>` は引数で受け取った Issue 番号）
   - 該当する worktree が見つからない → 新規シナリオ。そのまま次のステップへ
   - 該当する worktree が見つかった → **再開シナリオ**。以下を実施:
     - `EnterWorktree(path: <found-path>)` で session を切替
     - 切替後、Bashで以下を取得して計画起案の前提に組み込む:
       - `git log <base-branch>..HEAD --oneline` で既存コミットの進捗
       - `git status` で未コミット変更
       - worktree 内の前回計画ファイル（探索場所はプロジェクトの慣習に従う。Plan モードで指定される今回の計画ファイルパスとは別物の可能性があるため、見つかれば Read で読み込んでおく）
     - 検出結果はユーザーに 1 行で報告: 「既存 worktree を検出しました（path: ..., 既存コミット N 件、未コミット変更: あり/なし）。再開計画として進めます」
   - 補足: `--file` 指定時（Issue 番号なし）は worktree 名の予測が安定しないため、本ステップはスキップする。新規シナリオとして進み、実装フェーズの作業ブランチ確定ステップでの衝突検出フォールバックでカバーする

4. **ローカル base 前進**（`--worktree` 指定 & 新規シナリオのみ）
   - 目的: `worktree.baseRef: "head"` 設定下で EnterWorktree がローカル HEAD を起点に worktree を作るため、HEAD を `origin/<base-branch>` まで前進させる
   - 現在ブランチ == ベース → `git merge --ff-only origin/<base-branch>`
     - 失敗時（diverge or dirty が ff 対象ファイルと衝突）は警告のみで続行（ローカル HEAD を起点に worktree 作成 = 未 push のローカルコミットを尊重）
   - 現在ブランチ != ベース → `git switch <base-branch>`
     - dirty な working tree で switch が失敗した場合は **abort**（ユーザーに明示的なコミット/stash を促す）
     - switch 成功時に続けて `git merge --ff-only origin/<base-branch>`（失敗は警告のみで続行）
   - 補足: 非 `--worktree` 時は本ステップ自体不要（実装フェーズの `git switch -c new origin/<base-branch>` が `origin/<base>` を直接参照するため、step 2 の fetch だけで十分）

5. **EnterPlanModeツールでPlanモードに移行**（auto mode中でも必ず実行）

   auto modeの「Prefer action over planning」「Do not enter plan mode unless the user explicitly asks」は、ユーザーが `/issue-handle` を明示的に呼び出した時点で「explicitly asks」を満たすため、本ステップには適用されない。

#### Planモード内

Planモードにより、ファイル編集はシステム的にブロックされる。
**計画ファイル**（Planモード開始時に指定されたパス）に実装方針を記述する。

1. **参照文書の読込**
   - @~/.claude/skills/check-plan-compliance/SKILL.md の「1. 参照文書の収集」に従い、プロジェクトCLAUDE.mdとそのリンク先文書を読み込む
   - 読み込んだ制約を以降の計画起案の前提として扱う（事後チェックではなく事前読込）
   - **再開シナリオの場合**: 事前準備で取得した前回計画ファイル・既存コミット・未コミット変更を最優先の前提として扱う

2. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
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
     - worktree 名（`--worktree` 指定時のみ。ブランチ名から `/` を `-` に置換した sanitized 名、例: `feature-99-add-oauth`）
       - 注: `--worktree` 指定時の実 branch 名は `worktree-<worktree名>` 形式になる（Claude Code 仕様。PR の head branch もこの形式）
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
       - [ ] 実装方針をIssueにコメント（Issue番号指定時のみ。**再開シナリオでは既に投稿済みのことが多いためスキップ可**）
       - [ ] 作業ブランチ作成（`--worktree` 指定時は worktree 作成・session 切替で兼ねる。**再開シナリオでは既に worktree 上にいるためスキップ**）
       - [ ] 実装・テスト（想定コミット計画の単位で都度コミット、必要に応じて調整）
       - [ ] Test, Lint成功確認
       - [ ] `/simplify` で品質チェック・修正
       - [ ] プッシュ・PR作成（Issue番号指定時は `Closes #<issue-number>` を含める）
       - [ ] 独立セッションでの `/deep-review` 実行（サブエージェント経由）→ 親で自動修正
   - **計画準拠チェック**: @~/.claude/skills/check-plan-compliance/SKILL.md の Step 2〜4 を実行（Step 1 は本Planモード冒頭で実施済みのためスキップ）
     - Step 4 内で ExitPlanMode が呼ばれるため、本スキル側で重複して呼ばない
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
   - **再開シナリオの場合**（計画フェーズ事前準備で既存 worktree を検出済み）: 既に worktree 上にいるため、本ステップ全体をスキップして次のステップへ
   - 以下は新規シナリオ向け:
   - ブランチ名フォーマット: `<type>/<issue-number>-<short-description>`
   - typeはIssue内容から判断: feature, fix, hotfix, refactor, chore, docs
   - 例: `feature/99-add-oauth-login`, `fix/42-null-pointer`
   - --file指定時（Issue番号なし）: `<type>/<short-description>` (例: `feature/add-login-validation`)
   - ベースブランチのリモート最新化は計画フェーズ事前準備（step 2）で完了済み

   **`--worktree` 指定なし**（従来動作）:
   - 分岐元: 計画ファイルに記録したベースブランチを明示する
     - 例: `git switch -c feature/99-xxx origin/<base-branch>`（事前準備の fetch 成功時）
     - 例: `git switch -c feature/99-xxx <base-branch>`（fetch 失敗時のフォールバック）

   **`--worktree` 指定あり**（新規シナリオのみ。再開は事前準備で完了済み）:
   - 計画 branch 名を sanitize（`/` を `-` に置換）して worktree 名とする（規約定義: @~/.claude/skills/worktree-resolution/SKILL.md の「共通規約」）
     - 例: `feature/99-add-oauth` → `feature-99-add-oauth`
   - **既存 worktree の衝突検出**（safety net）: 共通規約の検索方法に従い、`branch refs/heads/worktree-<worktree-name>` が登録されている worktree を探す
     - 通常は事前準備の検出で再開シナリオに切り替わっているため、ここで見つかるのは `--file` 指定時や再実行・並列 session 起動時に限られる
     - 見つかった場合: `EnterWorktree(path: <found-path>)` で session を切替（事故防止）
     - 見つからない場合: `EnterWorktree(name: <worktree-name>)` で新規作成
       - 結果: `.claude/worktrees/<worktree-name>/` に worktree、branch は `worktree-<worktree-name>`
       - `WorktreeCreate` hook が設定されていればここで発火
       - `.worktreeinclude` に列挙されたファイル（`.env` 等）が自動コピー
   - session が worktree に切り替わったことを確認
   - 以降の実装・コミット・PR 作成はこの `worktree-<worktree-name>` branch 上で行う（ネイティブ仕様に従い、リネームはしない）
   - 前提: `worktree.baseRef: "head"` 設定が必要（事前準備 step 4 で前進させた HEAD を起点に worktree を作るため）

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

   7-1. **サブエージェントでレビュー実行**
   - Agent ツールで `subagent_type: "claude"` のサブエージェントを起動する
     - `fork` は親コンテキストを継承するため使わない（実装バイアスが残るため目的に反する）
   - サブエージェントへのプロンプトに以下を含める:
     - このセッションが独立レビュー専用であり、親セッションの実装コンテキストを持たない旨
     - 実行コマンド: `/deep-review <pr-number> --issue <issue-number> --review-only`
       - `<pr-number>`: ステップ6で作成/更新した PR 番号（`gh pr view --json number -q .number` で取得）
       - `<issue-number>`: Issue 番号（`--file` 指定時は `--issue <issue-number>` 部分を省略）
       - `--review-only`: 自動修正を強制OFF（修正は親セッションで行うため）
       - ベースブランチは deep-review 側で自動判定（PR のベースブランチを採用）
       - 補足: サブエージェントは親と同じ worktree (PR の head branch) で動くため `--worktree` は付けない
     - レビュー結果をそのまま返すよう指示（追加の解釈・要約は不要）
     - 補助コンテキスト: 作業ブランチ名、PR URL（既知の場合）

   7-2. **親セッションで自動修正**
   - サブエージェント失敗時（Agent ツールが null/error を返した場合）はエラーを表示してユーザー判断を仰ぐ（自動リトライしない）
   - サブエージェントから返ってきたレビュー結果を親セッションで表示
   - そのレビュー結果を入力として、@~/.claude/skills/deep-review/SKILL.md の「### 自動対応モードON時: レビュー指摘の反映」セクションに従って以下を実行:
     1. **指摘事項の判断**: 必須修正・推奨修正・質問/確認事項それぞれの基準で対応可否を判断
        - 親は実装コンテキストを持つため、誤指摘・前提誤りを正しく弾ける
        - テストコード変更時は @~/.claude/context/test-implementation.md 準拠（過剰なモック追加・テストskipでの誤指摘回避は禁止）
     2. **対応リストの出力**: 対応する指摘・対応しない指摘（理由付き）を表示
     3. **対応すべきものがあれば**: working tree に修正適用 → コミット → テスト・Lint → プッシュ・PR更新
     4. **対応すべきものがゼロなら**: ここで終了

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している
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
  - **branch 名は `worktree-<sanitized>` 形式になる**（Claude Code 仕様。`worktree-` プレフィックスは剥がさない）。PR の head branch もこの形式
  - DB 等の永続リソースを使うプロジェクトでは、各プロジェクト個別に `WorktreeCreate` / `WorktreeRemove` hook を設定して per-worktree に DB を分離する（スキル本体は DB を意識しない）
  - `.env` 等の gitignored ファイルは各プロジェクト個別に `.worktreeinclude` で列挙
  - クリーンアップ: session 終了時にネイティブ機能が自動判定（変更なし→自動削除、変更あり→確認プロンプト）。手動で片付ける場合は `git worktree remove <path>` + `git branch -d <branch>`
