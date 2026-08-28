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

## Issue階層（自動取得）
!`bash ~/.claude/scripts/issue-hierarchy.sh $0 2>/dev/null || echo "Issue階層の取得をスキップ（--file指定時、または取得失敗 — Issue番号指定時は要件確認で再実行する）"`

## 引数
- `<issue-number>`: 対応するIssue番号（`--file`と排他）
- `--file FILE_PATH`: 仕様ファイルのパス（`<issue-number>`と排他）
- `--base BRANCH`: ベースブランチを明示指定。省略時は起動時の現在ブランチ
- `--worktree`: 実装作業を専用の git worktree で隔離（並列開発時に推奨）。作成は同梱スクリプト、切替は `EnterWorktree(path:)`（詳細は事前準備 Step 3/5/6・注意事項）
- `--delegate-impl`: 実装フェーズの Step 3〜4（実装・テスト・コミットのループ）と Step 7-2 の決定済み修正の適用を `model: "sonnet"` の実装エージェントに委譲する。判断（計画・レビュー指摘の要否・修正方針）と外向き操作（Issue コメント・PR 作成・PR 説明更新・PR の Ready 化）は親セッションに残る（レビュー修正フェーズの push のみ実装エージェントが行う）
- `--no-plan-review`: 計画フェーズの計画検証（deep-plan-review）をスキップする。ドキュメント・spec の小修正や単一ファイルの軽微な変更など、計画に blocker の出る余地がほぼなく検証コストが見合わない Issue 向け。完了時の独立セッション `/deep-review` は省略されず、安全網として残る

## 前提条件
- Gitリポジトリ内で実行すること
- Issue番号指定時: `gh` CLIがインストール・認証済みであること
- **ベースブランチ**: `--base BRANCH` で明示指定 or 省略時は起動時の現在ブランチ
- `--worktree` 指定時: worktree はベースブランチ（`origin/<base>` 優先）から直接作成するため、メインツリーの現在ブランチ・dirty 状態には依存しない

## 実行内容

### 要件確認・調査
- Issue番号指定時: Issue本文・コメントを確認（起動時に取得済み。再取得する場合は `gh issue view <issue-number> --comments`）
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### Issue 階層の扱い（Issue番号指定時）

Issue と PR の対応は @~/.claude/skills/github-sub-issues/SKILL.md の「運用規約」に従う（葉 Issue = 1 PR、親 = リリース単位、親と合わせて読む、`release_manual_steps` 節、PR 本文の規則）。親本文の節をキーで引く手順は同スキルの「本文の節の読み取り」に従う。起動時に取得した `issue-hierarchy.sh` の出力（`kind` / `parent` / `sub_issues` / `siblings` / `all_sub_issues_closed` / `all_siblings_closed` / `blocked_by` / `blockers_closed` / `warnings`。契約の正はスクリプトヘッダー）で分岐する。取得できていなければ `bash ~/.claude/scripts/issue-hierarchy.sh <issue-number>` を実行する。`warnings[]` が空でない場合は判定に使う値が欠けている可能性があるため、内容を報告して以下の自動判定に頼らずユーザー確認へ倒す。

- **`standalone`**: 従来どおり単独の Issue として進める
- **`sub`（親あり・Sub なし）**: 実装対象。以下を要件確認に加える
  - **親の継承**: `gh issue view <parent.number> --comments` で親の本文・コメントを取得し、親の横断ルール・確定事項を本 Issue の要件と同格に扱う（運用規約「仕様の配置」）
  - **親 close 方針の記録**: 親本文の `release_manual_steps` 節（`github-sub-issues` の「本文の節の読み取り」の手順で引く）から `PR で閉じてよい`（「なし」マーカー）/ `PR で閉じない`（作業あり）を決めて計画ファイルに記録する（下記「計画完了」）。節が無い親は、この時点で `all_siblings_closed: true` なら推定 + 推奨を添えて AskUserQuestion で確認し、そうでなければ `未確定` と記録して PR 作成時に持ち越す（最後にならない Sub で毎回聞かない）
  - **依存の確認**: 判定の根拠は `blocked_by[]`（運用規約「Sub 間の順序」）。open の blocker があれば、その旨と影響（ベースブランチに依存先の PR head を使う stacked 構成になり、依存先マージ後に PR の base を付け替える必要がある）を示し、AskUserQuestion で続行可否とベースブランチの選択を確認する。続行時の選択を Step 1 のベースブランチに反映する。意図的な先行着手を妨げないため停止はしない
    - blocker が兄弟 Sub でなくても扱いは同じ（何が blocking かは GitHub の登録が正）。ただし stacked base に使える head branch が無い blocker では選択肢を続行 / 中断のみにする（兄弟でない blocker は PR を持たないことがある）。判定は `same_repo: false` か、`gh issue view <blocker.url> --json closedByPullRequestsReferences` が空か（`issue-hierarchy.sh` の `--with-prs` が Sub の PR を引くのと同じ機構）
    - `blocked_by` が空 = 依存が 1 件も登録されていない → **散文へフォールバック**する（運用規約の例外）。本 Issue の `depends_on` 節（または親の `composition` 節。いずれも同手順で引く）にある先行 Sub が `siblings[]` で open かを見る、従来どおりの確認
    - `blocked_by: null`（取得失敗）は上記冒頭の `warnings[]` の規定どおり、自動判定に頼らずユーザー確認へ倒す
- **`parent` / `parent_and_sub`（Sub あり）**: 実装対象ではない（Sub が実装単位）
  - open の Sub が残る（`all_sub_issues_closed: false`）→ **停止**。`bash ~/.claude/scripts/issue-hierarchy.sh <parent> --with-deps` で各 Sub の blocker を取り、Sub 一覧を番号・タイトル・状態で提示し、「次に着手できる Sub」を示して終了する。自動では着手しない（どの Sub をやるか・`--worktree` を使うかはユーザーの判断）
    - 対象は **open の Sub のみ**（closed の Sub は blocker がすべて closed でも着手可に含めない）。その上で Sub ごとに判定して 1 つの一覧にまとめる: `blocked_by` が空でない Sub は `blockers_closed: true` なら着手可、`blocked_by` が空の Sub（依存未登録）は親本文の `composition` 節（同手順で引く）の依存順で判定し、節が無い親では順序の制約なしとして着手可に含める。これにより一部の Sub だけリンク済みの親でも一覧が分裂しない
    - `blocked_by: null` の Sub は着手可に含めず、取得に失敗した旨を添える
  - 全 Sub が closed（`all_sub_issues_closed: true`）→ **親の充足検証 → close** を行う（下記）。計画フェーズ・実装フェーズには進まない

**親の充足検証 → close**（全 Sub 完了の親を渡されたとき）:
1. 事前準備 Step 1〜2 と同じ規則でベースブランチを確定し（`--base` / 現在ブランチ）、`origin/<base>` を fetch する
2. `bash ~/.claude/scripts/issue-hierarchy.sh <parent> --with-prs` で各 Sub を閉じた PR の状態を取り（`sub_issues[].prs[]` の `merged` / `base_ref`）、未マージ・`base_ref` がベースブランチと異なる・`prs` が空か null の Sub があれば警告し、続行するか AskUserQuestion で確認する
3. 親本文の受け入れ条件・横断ルール（と Sub の受け入れ条件のうち親に集約されているもの）を項目展開し、`origin/<base>` のコードと突き合わせて **充足 / 未実装 / 逸脱** に分類する（deep-review の「Issue 要件の充足状況」と同じ形式。差分ではなくベースブランチの現状を読む）
4. 未実装・逸脱が 1 つでもあれば close せず、充足表と未充足の内容を報告して終了する（対応は新しい Sub の起票等、ユーザーの判断）
5. 全充足なら `release_manual_steps` 節を確認する（同手順で引く）。「なし」マーカー（または節が無く手動作業も見当たらない）なら充足表を提示して close の承認を得てから閉じる: 充足表を scratchpad に Write して `gh issue comment <parent> -R <repo> --body-file <path>` で投稿（言語は Issue 本文に合わせる）→ `gh issue close <parent> -R <repo>`。手動作業ありなら、作業の完了をユーザーに確認できた場合のみ同じ手順で close し、未完了なら close せず作業一覧を提示して終了する

### 計画フェーズ

#### 事前準備（Planモード移行前、Bashで実行）

PlanモードではBashが使えないため、以下を**移行前に**必ず実行する。`--worktree` 指定時は Step 0-7 すべて、非 `--worktree` 時は Step 0/1/2/7 のみ実行（Step 3-6 はスキップ）。

**流れの要約**（`--worktree` 新規シナリオ）: 調査 (0) → base 確定・fetch (1-2) → 既存 worktree 検出 (3、あれば再開へ) → 名前確定・worktree 作成・切替 (4-6) → Plan モード (7)。worktree はベースブランチから直接作成し、メインツリーの状態（HEAD・working tree）には一切触れないため、Plan モード中もメインツリーで並列作業可能。

**Step 0. 要件確認・調査（最小限）**
- Issue 本文とコメント（`!gh issue view` で取得済み）を読み、続く Step 4 の worktree 名（type + description）判断に必要な範囲で関連コードを Read/Grep
  - コメントは時系列で読み、要件に影響する確定事項（スコープ調整・方針変更・仕様追記）は本文と同格の要件として扱う
  - Bot コメントと minimized なコメント（`isMinimized: true`）は読み飛ばす
- **深追い禁止**: 実装方針の詳細検討・計画起案は Plan モード内で実施（Plan モード内でも Read/Grep は可能、Bash のみ不可）
- 「### Issue 階層の扱い」の分岐（親なら停止または充足検証、Sub なら親の継承・親 close 方針・依存の確認）は**この Step で済ませる**（親 close 方針は AskUserQuestion を伴いうるため Plan モード前に確定させ、依存の選択は Step 1 のベースブランチに影響する）
- 参考: 上記「### 要件確認・調査」セクションは Plan モード内での追加調査時にも用いる共通の指針

**Step 1. ベースブランチの確定**
- `--base BRANCH` 指定時: その値を使用
- 省略時: `git branch --show-current` の値を使用
- 確定した値は計画ファイル記録用に控える

**Step 2. リモート最新化**: `git fetch origin <base-branch>` を常に実行
- 失敗時（リモート未設定等）は警告のみで続行

**Step 3. 既存 worktree 検出と再開判定**（`--worktree` 指定 & Issue番号指定時のみ）
- 同梱スクリプトで Issue 番号に対応する既存 worktree を検索する（現行命名・旧 EnterWorktree(name:) 方式の命名の両方に対応。パターンの正はスクリプトヘッダー）:
  ```bash
  bash ~/.claude/skills/issue-handle/scripts/create-worktree.sh detect <issue-number>
  ```
- `found: false` → 新規シナリオ。Step 4 へ進む
- `found: true` → **再開シナリオ**。以下を実施:
  - `EnterWorktree(path: <worktree_path>)` で session を切替（`<worktree_path>` / 以降の `<branch>` は detect の出力値）
  - **origin への同期**: `git fetch origin <branch>` の上で `git merge --ff-only origin/<branch>` を実行する（`<branch>` は検出した worktree の branch。別マシン・GitHub UI での suggestion コミット等による push があると、同期なしでは stale な HEAD 基準で再開計画を組んでしまうため。ahead — 未 push のローカル commit のみ — は no-op 成功する）
    - fetch が「リモートに branch が存在しない」理由で失敗 → 同期対象なしとして続行し、報告の origin 項目を「リモート branch なし」とする（未 push、またはマージ済みでリモート branch 削除済みのケース）
    - fetch がその他の理由（ネットワーク・認証等）で失敗 → 警告のみで続行し、報告の origin 項目を「同期未確認」とする（Step 2 の fetch 失敗時と同じ方針）
    - merge が失敗（diverge・dirty との衝突）→ 警告のみで続行し、報告の origin 項目を「乖離あり」とする（未 push のローカル作業を尊重しつつ、扱いは Plan モードでユーザーが判断できる）
    - 注: 「共通サブ手順: origin への同期」（worktree-resolution）を使わないのは意図的 — 同手順は ahead を停止条件とするが、再開では未 push のローカル commit が正常状態のため
  - 切替後、Bashで以下を取得して計画起案の前提に組み込む:
    - `git log <base-branch>..HEAD --oneline` で既存コミットの進捗
    - `git status` で未コミット変更
    - worktree 内の前回計画ファイル（探索場所はプロジェクトの慣習に従う。Plan モードで指定される今回の計画ファイルパスとは別物の可能性があるため、見つかれば Read で読み込んでおく）
  - 検出結果はユーザーに 1 行で報告: 「既存 worktree を検出しました（path: ..., 既存コミット N 件、未コミット変更: あり/なし、origin: 同期済み/リモート branch なし/乖離あり/同期未確認）。再開計画として進めます」
  - **Step 4-6 をスキップして Step 7（EnterPlanMode）へ**
- 補足: `--file` 指定時（Issue 番号なし）は worktree 名の予測が安定しないため、本ステップはスキップする。新規シナリオとして進み、実装フェーズの作業ブランチ確定ステップでの衝突検出フォールバックでカバーする

**Step 4. worktree 名確定**（`--worktree` 指定 & 新規シナリオのみ）
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
- **worktree 名は branch 名から `/` を `-` に置換した sanitized 形式**（例: `feature/99-add-oauth-login` → `feature-99-add-oauth-login`。スキル間で worktree を相互発見するための共通規約: @~/.claude/skills/worktree-resolution/SKILL.md の「共通規約」）

**Step 5. worktree 作成**（`--worktree` 指定 & 新規シナリオのみ）
- 同梱スクリプトで worktree と branch を作成する:
  ```bash
  bash ~/.claude/skills/issue-handle/scripts/create-worktree.sh create <worktree-name> <branch> <base-branch>
  ```
  （`<worktree-name>` は Step 4 の sanitized 名、`<branch>` は Step 4 の完全形式のブランチ名）
- 出力 JSON のうち本手順で使うフィールド（契約の正はスクリプトヘッダー）: `status` / `worktree_path` / `start_ref` / `warnings[]`
  - `status: ok` → Step 6 へ。`warnings[]` が空でなければ報告に併記し、`start_ref` がローカル base の場合はその旨も報告する
  - `status: branch_exists` / `path_exists` → **停止**してユーザー判断を仰ぐ（過去作業の残骸の可能性があり、破棄はユーザー確認なしに行わない。Step 3 の再開検出に掛からない片割れ残骸 — branch だけ・ディレクトリだけ — が典型）
  - 非ゼロ exit（base 不在等）→ stderr を提示して abort

**Step 6. EnterWorktree 実行**（`--worktree` 指定 & 新規シナリオのみ）
- `EnterWorktree(path: <worktree_path>)` で session を worktree に切り替える（`<worktree_path>` は Step 5 の出力値）
  - `EnterWorktree(name:)` を使わないのは base branch を指定できないため（経緯はスクリプトヘッダー参照）。path 入場のため session は worktree の owner にならず、終了時の自動クリーンアップ判定は働かない（後始末は「注意事項」参照）
- **失敗時のリカバリ**: session はまだメインツリーの cwd。Step 5 で作成した worktree・branch を片付けて（`git worktree remove <worktree_path>` + `git branch -D <branch>`。作成直後でコミット・変更なしのため安全）、ユーザーに失敗を通知して abort（原因究明はユーザーに委ねる）

**Step 7. EnterPlanModeツールでPlanモードに移行**（auto mode中でも必ず実行）

   auto modeの「Prefer action over planning」「Do not enter plan mode unless the user explicitly asks」は、ユーザーが `/issue-handle` を明示的に呼び出した時点で「explicitly asks」を満たすため、本ステップには適用されない。

#### Planモード内

Planモードにより、ファイル編集はシステム的にブロックされる。
**計画ファイル**（Planモード開始時に指定されたパス）に実装方針を記述する。

**新規シナリオで `--worktree` 指定時**: Plan モード冒頭でユーザーに 1 行報告する。

```
作業 worktree を作成し、branch `<branch>` で作業します（事前準備で完了済）。名前を変更したい場合はご指摘ください。
```

名前変更を希望されたら worktree 破棄 → 再作成で対応する: Plan モードを抜けて `ExitWorktree(action: "keep")` でメインツリーへ戻り、Step 6 の失敗時リカバリと同じ手順で破棄 → Step 4-6 を新しい名前で再実行 → 改めて EnterPlanMode。

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

3. **計画完了**
   - **計画ファイルに以下を含める**:
     - ブランチ名（typeを含む完全な形式）
     - ベースブランチ（取得済みの値）
     - Issue番号（Issue番号指定時。`gh issue comment` や `Closes #N` で使用）
     - 親 Issue 番号と親 close 方針（Issue が Sub の場合のみ）: `PR で閉じてよい`（`release_manual_steps` が「なし」マーカー、または節なしでユーザーが可と回答）/ `PR で閉じない`（手動作業あり、またはユーザーが否と回答）/ `未確定`（節なしで他の Sub が open のため未確認）。実装完了処理の PR 本文組み立てで参照する
     - worktree 使用（`--worktree` 指定時 true）
     - 実装委譲（`--delegate-impl` 指定時 true。実装フェーズと Step 7-2 の分岐判定に使う。再開シナリオでも今回の起動引数を正とし、前回計画の値は引き継がない）
     - worktree 名（`--worktree` 指定時のみ。ブランチ名から `/` を `-` に置換した sanitized 名、例: `feature-99-add-oauth`）
       - 注: branch 名はブランチ名（完全形式）をそのまま使う。再開シナリオでは Step 3 で検出した実 branch 名を正とする（旧命名の worktree もあるため）
     - worktree 作成状態（`--worktree` 指定時のみ）: 事前準備で完了済（新規シナリオ）／既存 worktree に切替済（再開シナリオ）
     - 言語方針（事前確認: コミット/PR は `git log` / `gh pr list --limit 5`、Issueコメントは Issue 本文・既存コメント、コードコメントは既存コードのコメント）:
       - コミット: 日本語 / 英語
       - PR（タイトル・本文）: 日本語 / 英語
       - Issueコメント: 日本語 / 英語（Issue番号指定時のみ記載）
       - コードコメント: 日本語 / 英語
       - （慣例が混在する場合のみ）判断根拠を1行で明記
       - 上記以外の成果物（README 等のドキュメント・コード内文字列等）は個別項目を設けず、書き込み先の既存内容の言語に合わせる（グローバル CLAUDE.md「成果物の言語」の原則）
     - 想定コミット計画（複数コミットになる場合のみ記述）:
       - 例:
         - コミット1: <内容>
         - コミット2: <内容>
       - 同じファイルに無関係な変更が混ざるのを防ぎ、各段階でテストを通せる単位に分ける
       - 実装中の調整は許容（厳密に固定しない）
     - シナリオ種別（事前準備の検出結果に基づく。`新規` / `再開`）
     - 実装手順チェックリスト:
       - [ ] 実装方針をIssueにコメント（Issue番号指定時のみ。取得済みコメントに自分の実装方針コメント — `viewerDidAuthor: true` かつ `## 実装方針` / `## Implementation plan` 見出し — があればスキップ。ただし今回の計画が投稿済み方針から実質的に変わる場合は更新版を投稿）
       - [ ] 作業ブランチ作成（`--worktree` 指定時は**事前準備で完了済のため本項目全体をスキップ**。非 `--worktree` 時のみ実装フェーズで実施）
       - [ ] 実装・テスト（想定コミット計画の単位で都度コミット、必要に応じて調整）
       - [ ] Test, Lint成功確認
       - [ ] `/simplify` で品質チェック・修正
       - [ ] プッシュ・PR作成（draft で作成。Issue番号指定時は `Closes #<issue-number>` を含める。Sub の場合は `Part of #<parent>` と、最後の Sub なら親の `Closes` も — 実装完了処理の規則に従う）
       - [ ] 独立セッションでの `/deep-review` 実行（`subagent_type: "independent-reviewer"` のサブエージェント経由）→ 親で自動修正
       - [ ] 同期検証を通過して PR を Ready 化
   - **計画準拠チェック**: @~/.claude/skills/check-plan-compliance/SKILL.md の Step 2〜4 を実行（Step 1 は本Planモード冒頭で実施済みのためスキップ。`--no-plan-review` 未指定時は後続の計画検証が ExitPlanMode を担うため、Step 4 の ExitPlanMode は呼ばず計画修正までに留める。`--no-plan-review` 指定時は後続の計画検証が無いため、同スキルの原則どおり Step 4 の ExitPlanMode まで実行する）
   - **計画検証**（`--no-plan-review` 未指定時のみ）: Skill ツールで `deep-plan-review` を起動する（引数: 計画ファイルパス）。`@` 参照で本文を先読みしない — 同スキルが依存する共有プロトコル（fresh-reader-verification）は同スキルの起動時に添付されるもので、起動を経ずに本文だけをなぞると未読のまま検証が回る（規約: skill-authoring「スキル間参照」）。修正後の計画での ExitPlanMode まで同スキルが担うため、本スキル側で重複して呼ばない
   - ユーザーの承認を待つ

4. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### 実装フェーズ

1. **実装方針をIssueにコメント**（Issue番号指定時のみ）
   - 見出し・本文とも計画の言語方針（Issueコメント）の言語で書く。会話の言語に引きずられない
     - 言語方針が英語の場合の見出し: `## Implementation plan` / `### Summary` / `### Main changes` / `### Affected files` / `### Branch`
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
     - ブランチ命名は事前準備 Step 4「worktree 名確定」の規約に従う（type enum、description ルール、フォーマット、`--file` 指定時の分岐すべて `--worktree` 有無に関わらず共通）
     - 分岐元: 計画ファイルに記録したベースブランチを明示する
       - 例: `git switch -c feature/99-xxx origin/<base-branch>`（事前準備の fetch 成功時）
       - 例: `git switch -c feature/99-xxx <base-branch>`（fetch 失敗時のフォールバック）
     - ベースブランチのリモート最新化は計画フェーズ事前準備（Step 2）で完了済み

#### 実装委譲（`--delegate-impl` 指定時のみ）

Step 3〜4 を実装エージェントに委譲する。本ブロック完了後は Step 3〜4 をスキップして Step 5（品質チェック）へ進む。

**起動**
- Agent ツールで `subagent_type: "claude"`・`model: "sonnet"` のサブエージェント（実装エージェント）を起動する（承認済み計画に沿った実装は判断密度が低く出力量が多いため下位モデルを充てる）
  - `fork` は使わない（model 指定が効かないため）
- 起動結果の agent ID を控える（以降の再開・照会・修正指示はすべて SendMessage で同一エージェントに送るため。agent ID はセッションをまたいで有効ではない）
- 実装エージェントは親と同じ session cwd（`--worktree` 時は worktree 内）で動くため、worktree・ブランチ操作の指示は不要

**プロンプトに含める契約**
- このセッションが実装専用であり、計画起案時の会話コンテキストを持たない旨
- 計画ファイルの絶対パス（Read して承認済み計画に従う）
- 作業範囲: 実装・テスト（想定コミット計画の単位で都度コミット）、Test/Lint 成功確認（失敗時は修正 → コミット → 再テスト）
  - コミットは @~/.claude/skills/git-commit/SKILL.md に従い、コミット・コードコメントの言語は計画の言語方針に従う（自動言語判定はスキップ）
  - テスト完了待ち等で後続単位の編集へ先行する場合も、編集を終えた計画単位は `git add <パス指定>` で先にコミットしてから進める（複数単位の未コミット変更を working tree に溜めない。実行中のテストが読むファイルと編集対象が重なる場合は先行編集しない）。計画が「テスト成功確認後にコミット」を指定していても、テスト待ちが発生する場合は先行コミットを優先し、テストで問題が出たら修正コミットを追加する（git-commit の自己完結原則の検証をテスト完了後へずらす意図的なトレードオフ）
- **長時間処理（コマンド・バックグラウンドタスク）の待ち方**: 完了待ちだけを目的にターンを終えない（サブエージェントのターン終了は親への報告として返り、再開に SendMessage の往復が必要になる。`TaskOutput` 相当のブロッキング取得ツールは無い前提で振る舞う）。この項はこの handoff を受けた実装エージェント向けで、親セッション（実装を委譲せず自ら実装ステップを回す場合を含む）は逆に、バックグラウンドのコマンド・タスク・エージェントの完了待ちではターンを終える。タスクの完了通知で自動再開されるため、親が no-op コマンドを反復してターンを延命する busy-wait はしない
  - 余裕を持って 10 分以内に終わる見込み → フォアグラウンド（`timeout` 上限の 600000ms まで指定可）で同一ターン内に完了させる
  - それ以外（上限に近い・超える見込み）→ バックグラウンド起動し、後続の実作業（次の計画単位の編集等）を進めた上で、ターンを終える前にプロセスの進行（CPU 時間・ログ更新）を確認する
  - 進行していないと確認できた場合 → 完了通知を待たず切り分けへ進み、解決できなければ停止条件のエスカレーション報告で停止する（起動直後で進行の判断材料が無い場合は通知待ちでよい）
- **既知のテスト失敗パターン**: ローカル環境依存で失敗・長時間化する既知事象（外部サービス未起動・DB スキーマの古さ・flaky 等）が親の知識（プロジェクト CLAUDE.md・memory・計画時の調査）にあれば handoff に含め、テスト失敗時の切り分け基準を与える（既知パターンに合致し本変更と無関係 → 実装報告に記録して続行 / 判断がつかない → 停止条件 (c) の 3 回試行を待たず、同じ報告フォーマットでエスカレーション）
- **禁止事項**: push・PR 作成・PR の Ready 化（`gh pr ready`）・/simplify・Issue コメントは行わない（親が実施。push は後続のレビュー修正フェーズで親が SendMessage で明示指示した場合のみ）
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
- ユーザーに停止を報告する場合、PR が既に作成済みなら 7-3 に到達していないため draft のまま残ることを明記する
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
   - コミット・PR・コードコメントの言語: 計画で確定した方針に従う（git-commit / git-pr の自動言語判定はスキップ）

4. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行
   - 数分以上かかる見込みの場合は `run_in_background: true` で実行（sleep ポーリングを避けキャッシュを節約）
   - **失敗した場合**: 修正 → コミット → 再テストを繰り返す
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

5. **品質チェック**
   - `/simplify` を実行し、変更コードの再利用性・品質・効率性を確認・修正
   - finding を見送る（skip する）場合、@~/.claude/skills/finding-triage/SKILL.md を読み、その規律で検証してから確定する（写像: /simplify の finding = 「対応が期待される指摘」。/simplify 組み込みスキルの skip 基準だけでは印象ベースの見送りを弾けないため）
   - 修正があればコミット

6. **実装完了処理**
   - 未コミットの変更があれば @~/.claude/skills/git-commit/SKILL.md に従ってコミット
   - @~/.claude/skills/git-pr/SKILL.md に従ってプッシュ・PR作成
     - 計画ファイルに記録したベースブランチを `/git-pr --draft --base <base-branch>` として引き渡す（レビューループ中は draft という不変条件。Ready 化は 7-3 のみが行う）
   - PR説明にIssue/仕様の背景・動機を含める（リンクだけでなく「なぜこの変更が必要か」を本文に書く）
   - Issue番号指定時: `Closes #<issue-number>` を含める
   - **Issue が Sub の場合**（計画ファイルに親 Issue 番号がある）: 運用規約「PR 本文」に従い `Part of #<parent>` を書く（`parent.same_repo: false` なら `Part of <parent.repo>#<parent>`。別リポの親は兄弟が取れず `all_siblings_closed` が false のままなので `Closes` は付かない）。**PR 作成直前に `bash ~/.claude/scripts/issue-hierarchy.sh <issue-number>` を再実行**し、`all_siblings_closed: true` かつ計画の親 close 方針が `PR で閉じてよい` なら `Closes #<parent>` も書く。方針が `未確定` なら、ここで親本文からの推定と推奨を添えて AskUserQuestion で確認してから決める。`warnings[]` が空でなければ `Closes #<parent>` は付けず、その旨を報告する
   - **draft 不変条件の確認**（PR 作成/更新後に無条件で実行。以降 `<pr-number>` は `gh pr view --json number -q .number`、`<owner/repo>` は `gh repo view --json nameWithOwner -q .nameWithOwner` で取り、7-3 でも使い回す）:
     - `gh pr view <pr-number> -R <owner/repo> --json isDraft -q .isDraft` が `false` なら `gh pr ready --undo <pr-number> -R <owner/repo>` で draft に戻し、戻した旨を1行報告する（ユーザー確認は取らない）。`/git-pr` は既存 PR の更新経路で draft 化しないため、再開シナリオ・手動作成の PR はここでしか回復できない
     - undo が失敗した場合は停止せず、レビューループ中も PR が draft でないことを警告として報告に残す

7. **独立セッションでのレビュー → 親での自動修正**
   - **目的（関心の分離）**:
     - **レビュー（発見）**: 実装バイアスを排除するため独立セッションで実施
     - **判断・修正**: 誤指摘・前提誤りを判別するため、実装コンテキストを持つ親セッションで実施
       - `--delegate-impl` 時、親は計画 + 実装報告 + diff を判断材料とし、実装経緯が必要で報告に無い場合は SendMessage で実装エージェントに照会して補う

   7-1. **サブエージェントでレビュー実行**
   - Agent ツールで `subagent_type: "independent-reviewer"` のサブエージェントを起動する（呼び出し時に `model` パラメータは指定しない。モデルは `~/.claude/agents/independent-reviewer.md` で固定されている）
     - `fork` は親コンテキストを継承するため使わない（実装バイアスが残るため目的に反する）
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
   - サブエージェント失敗時（Agent ツールが null/error を返した場合）はエラーを表示してユーザー判断を仰ぐ（自動リトライしない）。7-3 に到達しないため PR は draft のまま残る旨も報告に明記する
   - サブエージェントから返ってきたレビュー結果を親セッションで表示
   - そのレビュー結果を入力として、@~/.claude/skills/deep-review/SKILL.md の「自動対応モードON時: レビュー指摘の反映」セクションに従って以下を実行:
     1. **指摘事項の判断**: 必須修正・推奨修正・質問/確認事項・nit それぞれの基準で対応可否を判断
        - 親は実装コンテキストを持つため、誤指摘・前提誤りを正しく弾ける（`--delegate-impl` 時は実装報告を判断材料に加える）
        - テストコード変更時は @~/.claude/skills/test-implementation/SKILL.md 準拠（過剰なモック追加・テストskipでの誤指摘回避は禁止）
     2. **対応リストの出力**: 対応する指摘・対応しない指摘（理由付き）を表示
        - `--delegate-impl` 時は対応する各指摘に修正方針（どう直すか）まで含める（マイクロな実装判断まで実装エージェントに落とさないため）
     3. **対応すべきものがあれば**: working tree に修正適用 → コミット → テスト・Lint → プッシュ・PR更新 → 7-3 へ進む
        - 非 `--delegate-impl` 時: 親が実施
        - `--delegate-impl` 時: 対応リスト（指摘 + 修正方針）を SendMessage で実装エージェントに送り、修正適用 → コミット → テスト・Lint → プッシュまでを実施させる（実装委譲ブロックの禁止事項のうち push をここで解除する。停止条件は実装委譲ブロックと同じ）。完了後、親が修正コミットの diff を確認して意図どおりか検証し、PR 説明の更新（要否判断を含む）は親が実施する
          - 対応する指摘がすべて軽微・機械的（typo 等の1行修正レベル）な場合は、SendMessage の往復コストを避けて親が直接適用してよい
          - 実装エージェントが失われている場合（SendMessage 失敗・セッション再開後等）は、新しいサブエージェントを起こさず親が自ら適用する
     4. **対応すべきものがゼロなら**: 修正・コミット・PR 更新は行わず 7-3 へ進む（参照先 deep-review の同セクションは「ここで終了」で閉じるが、本スキルではその先に 7-3 がある）

   7-3. **PR を Ready 化**
   - **7-2 の完了後は常に実行する**（指摘を適用・プッシュした場合も、対応すべきものがゼロだった場合も）。`--delegate-impl` 時も親セッションが実行する（実装エージェントには行わせない）
   - 先に同期を検証し、1つでも満たさなければ **Ready 化せず**、満たさなかった項目と PR が draft のまま残ることを報告して停止する（レビュー修正がローカルにだけ・PR が追わない branch にだけ存在する状態で「Ready」を宣言しないための門）:
     ```bash
     git status --porcelain                                                  # 空であること
     git fetch origin <branch> && git rev-parse HEAD origin/<branch>         # 成功し、2行が同一 commit であること
     gh pr view <pr-number> -R <owner/repo> --json headRefOid -q .headRefOid # 上と同一 commit であること
     ```
     - `headRefOid` はプッシュ直後に数秒遅れることがあるため、**これのみ**が不一致なら数秒後に一度だけ再取得する。working tree の汚れ・HEAD と `origin/<branch>` の不一致は遅延ではないので再試行せず報告する
     - 検証するのはコミットの同期のみ。テスト・Lint は 7-2 でプッシュ前に実行済みのため再実行しない
   - 検証を通過したら `gh pr ready <pr-number> -R <owner/repo>` を実行し、Ready 化した旨を報告する

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している（ローカル環境依存の既知失敗と切り分け済みのものは、実装報告への記録をもって充足とする）
- [ ] 変更がプッシュされている
- [ ] PRが作成されている（または既存PRが更新されている）
- [ ] 独立セッションでの `/deep-review` を実施し、結果を親セッションで表示済み
- [ ] レビュー指摘のうち親が「対応する」と判断したものは適用・コミット・プッシュ済み（対応すべきものがゼロなら何もしない）
- [ ] 同期検証を通過して PR を Ready 化済み

## 注意事項
- **Issue への追加コメント**: フォーマット定義外の投稿（検証報告・実施記録等）も計画の言語方針（Issueコメント）の言語で書く。会話の言語に引きずられない
- **PR は draft で作られる**: レビューループ（7-1〜7-2）が終わるまで PR は draft のままで、7-3 の同期検証を通過した時点でのみ Ready 化する。エスカレーション・レビュー失敗・ユーザー中断で途中停止した場合は意図的に draft のまま残る（レビュー由来の修正が入る前にマージされる窓を塞ぐため）
- **Planモード中**: ファイル編集・ブランチ作成はシステム的にブロックされる
- **auto mode下での運用**: auto modeであっても計画フェーズ（EnterPlanMode）はスキップしない
- **テスト失敗時**: 修正 → コミット → 再テストのサイクルを繰り返す
- **`--worktree` 指定時の前提・挙動**:
  - 並列で複数 issue を進める場合、issue 1 つにつき 1 つの Claude session（別ターミナル/別 tmux ペイン）が必要
  - worktree はベースブランチから直接作成し、メインツリーの状態（HEAD・working tree）には一切触れない。Plan モード中もメインツリーで並列の別作業が可能
  - **branch 名はブランチ名（完全形式、例: `feature/99-add-oauth`）をそのまま使う**。PR の head branch もこの形式（旧命名 `worktree-<sanitized>` の既存 worktree は Step 3 の再開検出が拾う）
  - `.env` 等の gitignored ファイルは各プロジェクト個別に `.worktreeinclude` で列挙する（コピーは create-worktree.sh がネイティブ挙動を再現）
  - **`WorktreeCreate` hook は発火しない**（`git worktree add` 直接作成のため）。hook で worktree 環境を構築するプロジェクト（非 git VCS、per-worktree の DB 分離等）は本スキルの `--worktree` の対象外で、必要なら hook 相当のセットアップを手動実行する（スキル本体は DB を意識しない）
  - クリーンアップ: path 入場のため session は worktree の owner にならず、終了時の自動クリーンアップ判定（変更なし→自動削除等）は働かない。マージ後の回収は `/cleanup-merged`、手動で片付ける場合は `git worktree remove <path>` + `git branch -d <branch>`
- **`--delegate-impl` の前提と限界**: deep-plan-review が保証する計画の自己完結性が handoff 品質の前提となる。deep-review の指摘数やエスカレーションが目立って増える場合は、委譲をやめる前に計画側の粒度（blocker 基準を「Sonnet が迷わない」水準へ締める）を疑う
- **`--no-plan-review` と `--delegate-impl` の併用は非推奨**: 上記のとおり計画の自己完結性検証が handoff 品質の前提のため、未検証の計画を Sonnet へ委譲すると品質低下リスクが上がる。両方が指定された場合はその旨を警告した上で続行する（停止はしない）
