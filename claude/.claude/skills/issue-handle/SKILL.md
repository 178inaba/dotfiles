---
name: issue-handle
description: Issueの調査から実装完了までを一貫して対応
argument-hint: <issue-number | --file FILE_PATH> [--base BRANCH] [--worktree] [--no-plan-review]
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
/issue-handle 99 --no-plan-review             # 軽微な Issue 向けに計画検証（deep-plan-review）を省略
```

## Issue情報（自動取得）
!`gh issue view $0 --json title,body,labels,assignees,comments 2>/dev/null || echo "Issue情報の取得をスキップ（--file指定時）"`

## Issue階層（自動取得）
!`ccx issue tree $0 2>/dev/null || echo "Issue階層の取得をスキップ（--file指定時、または取得失敗 — Issue番号指定時は要件確認で再実行する）"`

## 引数
- `<issue-number>`: 対応するIssue番号（`--file`と排他）
- `--file FILE_PATH`: 仕様ファイルのパス（`<issue-number>`と排他）
- `--base BRANCH`: ベースブランチを明示指定。省略時は起動時の現在ブランチ
- `--worktree`: 実装作業を専用の git worktree で隔離（並列開発時に推奨）。手順は [references/worktree.md](references/worktree.md)
- `--no-plan-review`: 計画フェーズの計画検証（deep-plan-review）をスキップする。ドキュメント・spec の小修正や単一ファイルの軽微な変更向け。同スキルの承認後の同期点も無くなり、承認がそのまま実装の開始になる。完了時の独立セッション `/deep-review` は省略しない

## 前提条件
- Gitリポジトリ内で実行すること
- Issue番号指定時: `gh` CLIがインストール・認証済みであること
- **ベースブランチ**: `--base BRANCH` で明示指定 or 省略時は起動時の現在ブランチ
- `--worktree` 指定時: worktree はベースブランチ（`origin/<base>` 優先）から作成し、メインツリーの現在ブランチ・dirty 状態に依存しない

## 実行内容

### 要件確認・調査
- Issue番号指定時: Issue本文・コメントを確認（起動時に取得済み。再取得する場合は `gh issue view <issue-number> --comments`）
- --file指定時: Readツールで仕様ファイルを読み込み
- 関連コードを調査し、実装方針を検討

### Issue 階層の扱い（Issue番号指定時）

Issue と PR の対応は、Skill ツールで `github-sub-issues` を起動し、その「運用規約」に従う。親本文の節をキーで引く手順は同スキルの「本文の節の読み取り」に従う。起動時に取得した `ccx issue tree` の出力の `kind` で分岐する（出力の読み方は `ccx issue tree --help`）。取得できていなければ `ccx issue tree <issue-number>` を実行する。**本節と [references/parent.md](references/parent.md) が実行する `ccx issue tree` はいずれも、非ゼロ exit したら stderr を提示して停止する**（取得できなかった分岐を飛ばして進めない）。`warnings[]` が空でなければ内容を報告し、以下の自動判定に頼らずユーザー確認へ倒す。

- **`standalone`**: 単独の Issue として進める
- **`sub`（親あり・Sub なし）**: 実装対象。以下を要件確認に加える
  - **親の継承**: `gh issue view <parent.number> --comments` で親の本文・コメントを取得し、親の横断ルール・確定事項を本 Issue の要件と同格に扱う（運用規約「仕様の配置」）
  - **親 close 方針の記録**: 親本文の `release_manual_steps` 節（`github-sub-issues` の「本文の節の読み取り」の手順で引く）から `PR で閉じてよい`（「なし」マーカー）/ `PR で閉じない`（作業あり）を決めて計画ファイルに記録する（下記「計画完了」）。節が無い親は、この時点で `all_siblings_closed: true` なら推定 + 推奨を添えて AskUserQuestion で確認し、そうでなければ `未確定` と記録して PR 作成時に持ち越す
  - **依存の確認**: 判定の根拠は `blocked_by[]`（運用規約「Sub 間の順序」）。open の blocker があれば、その旨と影響（ベースブランチに依存先の PR head を使う stacked 構成になり、依存先マージ後に PR の base を付け替える必要がある）を示し、AskUserQuestion で続行可否とベースブランチの選択を確認する。続行時の選択を Step 1 のベースブランチに反映する。停止はしない
    - blocker が兄弟 Sub でなくても扱いは同じ。ただし stacked base に使える head branch が無い blocker では選択肢を続行 / 中断のみにする。判定は `same_repo: false` か、`gh issue view <blocker.url> --json closedByPullRequestsReferences` が空か
    - `blocked_by` が空 = 依存が 1 件も登録されていない → **散文へフォールバック**する（運用規約の例外）。本 Issue の `depends_on` 節（または親の `composition` 節。いずれも同手順で引く）にある先行 Sub が `siblings[]` で open かを見る
- **`parent` / `parent_and_sub`（Sub あり）**: 実装対象ではない。[references/parent.md](references/parent.md) に従う（着手可能な Sub の提示、または親の充足検証 → close）

### 計画フェーズ

#### 事前準備（Planモード移行前、Bashで実行）

以下は **Plan モード移行前に**必ず実行する。`--worktree` 指定時は Step 0-7 すべて、非 `--worktree` 時は Step 0/1/2/7 のみ実行（Step 3-6 はスキップ）。

`--worktree` 指定時は、Step 0 の前に [references/worktree.md](references/worktree.md) を読む（以降のステップの行では読み直さない）。

**Step 0. 要件確認・調査（最小限）**
- Issue 本文とコメント（`!gh issue view` で取得済み）を読み、続く Step 4 の worktree 名（type + description）判断に必要な範囲で関連コードを Read/Grep
  - コメントは時系列で読み、要件に影響する確定事項（スコープ調整・方針変更・仕様追記）は本文と同格の要件として扱う
  - Bot コメントと minimized なコメント（`isMinimized: true`）は読み飛ばす
- **深追い禁止**: 実装方針の詳細検討・計画起案は Plan モード内で実施
- 「### Issue 階層の扱い」の分岐は**この Step で済ませる**

**Step 1. ベースブランチの確定**
- `--base BRANCH` 指定時: その値を使用
- 省略時: `git branch --show-current` の値を使用
- 確定した値は計画ファイル記録用に控える

**Step 2. リモート最新化**: `git fetch origin <base-branch>` を常に実行
- 失敗時（リモート未設定等）は警告のみで続行

**Step 3. 既存 worktree の残骸判定**（`--worktree` 指定 & Issue番号指定時のみ）
- [references/worktree.md](references/worktree.md) の事前準備 Step 3 に従う

**Step 4. worktree 名確定**（`--worktree` 指定時のみ）
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
- **worktree 名は branch 名から `/` を `-` に置換した sanitized 形式**（例: `feature/99-add-oauth-login` → `feature-99-add-oauth-login`。`worktree-resolution` の「共通規約」）

**Step 5. worktree 作成**（`--worktree` 指定時のみ）
- [references/worktree.md](references/worktree.md) の事前準備 Step 5 に従う

**Step 6. EnterWorktree 実行**（`--worktree` 指定時のみ）
- [references/worktree.md](references/worktree.md) の事前準備 Step 6 に従う

**Step 7. EnterPlanModeツールでPlanモードに移行**（auto mode中でも必ず実行。`/issue-handle` の明示的な呼び出しが「explicitly asks」を満たす）

#### Planモード内

**計画ファイル**（Planモード開始時に指定されたパス）に実装方針を記述する。

**`--worktree` 指定時**: 冒頭で [references/worktree.md](references/worktree.md) の「Planモード内」に従って報告する。

1. **参照文書の読込**
   - `ccx plan docs` を、Issue の `affected_code` 節が挙げるパスを引数にして実行する（出力の読み方は `ccx plan docs --help`）。渡すのは**バッククォートで囲まれたパスだけ**にする。`--file` 指定時は仕様ファイルが挙げるパスを同じ形で渡す
   - `loaded[]` と `documents[]` の両方が空なら、この読込は対象なしとして飛ばす
   - `documents[]` の各パスを Read で読み、`warnings[]` の各項目を報告して続行する（`loaded[]` は読み直さない）
   - 読み込んだ制約を以降の計画起案の前提として扱う

2. **ユーザーとの対話**
   - 不明点があればAskUserQuestionでユーザーに質問
   - Issue本文とコメントで要件が矛盾し、スレッド内で結論が確定していない場合はAskUserQuestionで確認する（結論が明確に出ている矛盾は確認不要、コメント側を採用）
   - 実装方針をユーザーに提示
   - フィードバックを受けて方針を調整

3. **計画完了**
   - **計画ファイルに以下を含める**:
     - ブランチ名（typeを含む完全な形式）
     - ベースブランチ（取得済みの値）
     - Issue番号（Issue番号指定時）
     - 親 Issue 番号と親 close 方針（Issue が Sub の場合のみ）: `PR で閉じてよい`（`release_manual_steps` が「なし」マーカー、または節なしでユーザーが可と回答）/ `PR で閉じない`（手動作業あり、またはユーザーが否と回答）/ `未確定`（節なしで他の Sub が open のため未確認）
     - worktree 使用（`--worktree` 指定時 true）
     - worktree 名（`--worktree` 指定時のみ。Step 4 の sanitized 名）
     - 言語方針（事前確認: コミット/PR は `git log` / `gh pr list --limit 5`、コードコメントは既存コードのコメント）:
       - コミット: 日本語 / 英語
       - PR（タイトル・本文）: 日本語 / 英語
       - コードコメント: 日本語 / 英語
       - （慣例が混在する場合のみ）判断根拠を1行で明記
       - 上記以外の成果物は個別項目を設けず、書き込み先の既存内容の言語に合わせる
     - 想定コミット計画（複数コミットになる場合のみ記述）:
       - 例:
         - コミット1: <内容>
         - コミット2: <内容>
       - 同じファイルに無関係な変更が混ざらず、各段階でテストを通せる単位に分ける
     - 実装手順チェックリスト:
       - [ ] 作業ブランチ作成（非 `--worktree` 時のみ）
       - [ ] 実装・テスト（想定コミット計画の単位で都度コミット、必要に応じて調整）
       - [ ] Test, Lint成功確認
       - [ ] `/simplify` で品質チェック・修正
       - [ ] プッシュ・PR作成（draft で作成。Issue番号指定時は `Closes #<issue-number>` を含める。Sub の場合は `Part of #<parent>` と、最後の Sub なら親の `Closes` も — 実装完了処理の規則に従う）
       - [ ] 独立セッションでの `/deep-review` 実行（`subagent_type: "independent-reviewer"` のサブエージェント経由）→ 親で自動修正
       - [ ] 同期検証を通過して PR を Ready 化
   - **計画準拠チェック**: Skill ツールで `check-plan-compliance` を、`--no-plan-review` の有無にかかわらず引数 `--no-exit` で起動する
   - **参照・コマンドチェック**: 計画準拠チェックの**後**に `ccx plan check <計画ファイルパス>` を実行する
     - finding は計画の著者が解消する: 参照を直す / 新規に作る成果物なら `(new)` または `（新規）` を注記する / コマンドを実行して結果を記録する（実行できないなら理由を記録する。形式は `ccx plan check --help`）。直したら再実行し、**clean になるまで繰り返す**
     - finding が残る計画で、計画検証を起動しない・`ExitPlanMode` を呼ばない
   - **計画検証**（`--no-plan-review` 未指定時のみ）: Skill ツールで `deep-plan-review` を起動する（引数: 計画ファイルパス）。ExitPlanMode は同スキルが呼ぶので、本スキルからは呼ばない
   - **ExitPlanMode**（`--no-plan-review` 指定時のみ）: 参照・コマンドチェックが clean になった時点で本スキルが `ExitPlanMode` を呼ぶ
   - ユーザーの承認を待つ

4. **実装フェーズへ**（承認後）
   - 以下の「実装フェーズ」を実行

### 実装フェーズ

**開始前の同期点**（`--no-plan-review` 未指定時）: `deep-plan-review` の承認後の同期点が完了して同スキルが返るまで、以下のステップを 1 つも開始しない。同期点で「打ち切り」が選ばれた場合は本 run をそこで終え、この時点で作られているもの（worktree・ブランチ・計画ファイル）はそのまま残す。

1. **作業ブランチ確定**
   - **`--worktree` 指定時**: 本ステップ全体をスキップして次のステップへ
   - **非 `--worktree` 時のみ以下を実施**:
     - ブランチ命名は事前準備 Step 4「worktree 名確定」の規約に従う（`--file` 指定時の分岐を含め、すべて `--worktree` 有無に関わらず共通）
     - 分岐元: 計画ファイルに記録したベースブランチを明示する
       - 例: `git switch -c feature/99-xxx origin/<base-branch>`（事前準備の fetch 成功時）
       - 例: `git switch -c feature/99-xxx <base-branch>`（fetch 失敗時のフォールバック）

2. **実装・テスト修正**
   - **想定コミット計画の単位で都度コミット**（実装 → テスト確認 → コミット）。計画と現実が乖離したらコミット境界を調整してよい
   - テストコードの作成・修正では Skill ツールで `test-implementation` を起動し、その3原則に従う
   - コミットは Skill ツールで `git-commit` を起動して行う
   - コミット・PR・コードコメントの言語: 計画で確定した方針に従う（git-commit / git-pr の自動言語判定はスキップ）

3. **Test, Lint成功確認**
   - プロジェクトのテスト・Lintコマンドを実行
   - 数分以上かかる見込みの場合は `run_in_background: true` で実行
   - **失敗した場合**: 修正 → コミット → 再テストを繰り返す
   - 例: `make all`, `npm test && npm run lint`, `go test ./... && golangci-lint run`

4. **品質チェック**
   - `/simplify` を実行し、変更コードの再利用性・品質・効率性を確認・修正
     - レビューエージェントは `model` を指定せず（親継承）、`isolation: "worktree"` で隔離し、4 角度（reuse / simplification / efficiency / altitude）を 4 エージェントのまま起動する。diff が小さいことを理由に角度を統合・削減しない（角度の構成が変わっていたら組み込み側が正で、統合・削減しない点だけが不変）
     - `/simplify` の要約はターンの終わりではない。finding の見送り検証と修正のコミットまで済ませたら、ユーザー確認を待たず同一ターンでステップ5へ進む（`ccx hook issue-handle-guard` が強制する。バックグラウンド待ちでターンを終える規定はそのまま適用される）
   - finding を見送る（skip する）場合、Skill ツールで `finding-triage` を起動し、その規律で検証してから確定する（写像: /simplify の finding = 「対応が期待される指摘」）
   - 修正があればコミット

5. **実装完了処理**
   - 未コミットの変更があれば Skill ツールで `git-commit` を起動してコミット
   - Skill ツールで `git-pr` を引数 `--draft --base <base-branch>` で起動し、プッシュ・PR作成を行う（`<base-branch>` は計画ファイルに記録したベースブランチ。Ready 化は 6-3 のみが行う）
   - PR説明にIssue/仕様の背景・動機を含める（リンクだけでなく「なぜこの変更が必要か」を本文に書く）
   - Issue番号指定時: `Closes #<issue-number>` を含める
   - **Issue が Sub の場合**（計画ファイルに親 Issue 番号がある）: 運用規約「PR 本文」に従い `Part of` と親の `Closes` を書く。**PR 作成直前に `ccx issue tree <issue-number>` を再実行**し、その値と計画の親 close 方針で判定する。方針が `未確定` なら、ここで親本文からの推定と推奨を添えて AskUserQuestion で確認してから決める。`warnings[]` が空でなければ `Closes #<parent>` は付けず、その旨を報告する
   - **draft 不変条件の確認**（PR 作成/更新後に無条件で実行。`gh pr view --json number,isDraft` と `gh repo view --json nameWithOwner -q .nameWithOwner` で `<pr-number>` / `<owner/repo>` を確定し、6-1・6-3 でも取り直さず使い回す）:
     - `isDraft` が `false` なら `gh pr ready --undo <pr-number> -R <owner/repo>` で draft に戻し、戻した旨を1行報告する（ユーザー確認は取らない）
     - undo が失敗した場合は停止せず、レビューループ中も PR が draft でないことを警告として報告に残す

6. **独立セッションでのレビュー → 親での自動修正**

   6-1. **サブエージェントでレビュー実行**
   - Agent ツールで `subagent_type: "independent-reviewer"` のサブエージェントを起動する（`model` パラメータは指定しない）
     - `fork` は使わない
   - サブエージェントへのプロンプトに以下を含める:
     - このセッションが独立レビュー専用であり、親セッションの実装コンテキストを持たない旨
     - Skill ツールで `deep-review` を引数 `<pr-number> --issue <issue-number> --no-autofix` で起動すること
       - `<pr-number>`: ステップ5で確定した PR 番号
       - `<issue-number>`: Issue 番号（`--file` 指定時は `--issue <issue-number>` 部分を省略）
       - `--worktree` は付けない
     - レビュー結果をそのまま返すよう指示（追加の解釈・要約は不要）
     - 補助コンテキスト: 作業ブランチ名、PR URL（既知の場合）

   6-2. **親セッションで自動修正**
   - サブエージェント失敗時（Agent ツールが null/error を返した場合）はエラーを表示してユーザー判断を仰ぐ（自動リトライしない）。6-3 に到達しないため PR は draft のまま残る旨も報告に明記する
   - サブエージェントから返ってきたレビュー結果を親セッションで表示
   - そのレビュー結果を入力として、Skill ツールで `finding-severity` を起動し、その判断基準・対応リストの形式に従って対応要否を判断し、対応リストを出力する
   - 対応リストの確定後:
     - **「Issue 側の修正が要る指摘」が空でなければ**、以下の分岐より前に [references/issue-conflict.md](references/issue-conflict.md) を読んでユーザーに確認し、回答で各指摘を「対応する指摘」か「対応しない指摘」へ移す
     1. **対応すべきものがあれば**: working tree に修正適用 → コミット → テスト・Lint → Skill ツールで `git-pr` を引数 `--base <base-branch>` で起動してプッシュ・PR更新 → 6-3 へ進む
     2. **対応すべきものがゼロなら**: 修正・コミット・PR 更新は行わず 6-3 へ進む

   6-3. **PR を Ready 化**
   - **6-2 の完了後は常に実行する**（指摘を適用・プッシュした場合も、対応すべきものがゼロだった場合も）
   - `ccx pr ready <pr-number>` を実行する（出力の読み方は `ccx pr ready --help`）
     - `status` が `ready` / `already_ready` → PR が Ready である旨を報告する
     - それ以外の `status` → 落ちた検査と、PR が draft のまま残ることを報告して**停止**する
     - 非ゼロ exit → stderr を提示して**停止**する
   - テスト・Lint は再実行しない

## 完了条件
以下をすべて満たした時点で完了:
- [ ] 実装が完了している
- [ ] テスト・Lintが成功している
- [ ] 変更がプッシュされている
- [ ] PRが作成されている（または既存PRが更新されている）
- [ ] 独立セッションでの `/deep-review` を実施し、結果を親セッションで表示済み
- [ ] レビュー指摘のうち親が「対応する」と判断したものは適用・コミット・プッシュ済み（対応すべきものがゼロなら何もしない）
- [ ] 同期検証を通過して PR を Ready 化済み

## 注意事項
- **PR は draft で作られる**: レビューループ（6-1〜6-2）が終わるまで PR は draft のままで、6-3 の同期検証を通過した時点でのみ Ready 化する。エスカレーション・レビュー失敗・ユーザー中断で途中停止した場合は draft のまま残す
- **`--worktree` 指定時の前提・挙動**: [references/worktree.md](references/worktree.md) の「注意事項」
