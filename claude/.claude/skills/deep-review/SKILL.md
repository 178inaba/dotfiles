---
name: deep-review
description: コード差分を詳細にレビュー
argument-hint: "[<pr-number>] [--issue NUMBER] [--worktree] [--local-only] [--review-only]"
---

# /deep-review

コード差分を経験豊富なシニアエンジニアとして、本番運用を考慮しながら詳細にレビューし、Approve可能か判断

## 使用方法
```
/deep-review [<pr-number>] [--issue ISSUE_NUMBER] [--worktree] [--local-only] [--review-only]
```

**引数**:
- `<pr-number>`: 対象 PR 番号（省略時はカレント branch の PR を `gh pr view` で推論。`--worktree` 指定時の主用途）
- `--issue ISSUE_NUMBER`: 指定したIssue要件を満たしているか確認
- `--worktree`: 対象 PR の worktree に切替（既存があれば再利用、無ければ作成）。並列で別作業中に他者の PR をレビューする際の主要ユースケース
- `--local-only`: 強制的にローカル出力のみ（PRコメント投稿しない）
- `--review-only`: 強制的に自動対応モードOFF（修正・コミット・プッシュしない、レビュー結果出力のみ）

ベースブランチは自動判定: PRがあればPRのベースブランチ、なければリポジトリのデフォルトブランチを使用する（フォールバックは `origin/main`）。

**例**:
```
/deep-review                              # カレント branch の差分をレビュー
/deep-review --issue 123                  # Issue #123の要件も確認
/deep-review --local-only                 # 他人のPRでもローカル出力のみ
/deep-review --issue 123 --review-only    # 自動修正を行わずレビュー結果のみ出力（サブエージェント経由レビュー想定）
/deep-review 123 --worktree               # PR 123 を worktree に切替してレビュー（並列レビューの主用途）
/deep-review 123 --worktree --review-only # 他人 PR を独立 worktree で読み専レビュー
```

## 実行内容

### 1. 引数の解析
- 第1引数（`--`で始まらない数値）があれば PR 番号として使用（`<pr-number>`）
- `--issue ISSUE_NUMBER`: Issue番号を抽出
- `--worktree`: worktree 切替フラグを設定（後続の「Worktree 解決」セクション参照）
- `--local-only`: ローカル出力のみフラグを設定
- `--review-only`: 自動対応モード強制OFFフラグを設定

ベースブランチは引数で受け付けず、後続の差分取得時に自動判定する:
1. `gh pr view --json baseRefName --jq '.baseRefName'` でPRのベースブランチを取得し、`origin/` を付加
2. PRがなければ `git symbolic-ref refs/remotes/origin/HEAD` でデフォルトブランチを取得
3. いずれも取得できなければ `origin/main` をフォールバック

**解析例**:
```
/deep-review --issue 456
→ pr-number: null, issue: 456, worktree: false

/deep-review --local-only
→ pr-number: null, local-only: true

/deep-review --issue 123 --review-only
→ pr-number: null, issue: 123, review-only: true

/deep-review 123 --worktree
→ pr-number: 123, worktree: true

/deep-review 123 --worktree --review-only
→ pr-number: 123, worktree: true, review-only: true
```

### 1.5. Worktree 解決（`--worktree` 指定時のみ、引数解析直後に実行）

`/review-response --worktree` と同一規約で worktree を解決する。

1. **PR 番号確定**
   - `<pr-number>` が指定されていればそれを使用
   - 無ければ `gh pr view --json number -q .number` でカレント branch の PR を推論
     - 推論失敗時はエラーで停止し、ユーザーに `<pr-number>` の明示指定を促す

2. **PR の head branch 名取得**
   - `gh pr view <PR> --json headRefName -q .headRefName`

3. **worktree 名の計算**
   - branch 名から `/` を `-` に置換（`/issue-handle --worktree`・`/review-response --worktree` と同一規約）
   - 例: `feature/99-add-oauth` → `feature-99-add-oauth`

4. **既存 worktree の検索**
   - `git worktree list --porcelain` を解析
   - `branch refs/heads/<pr-branch>` が登録されている worktree を探す
   - 見つかればそのパスを記録

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
   - worktree 内で `git switch <pr-branch>` で PR の実 branch に切替
     - local に同名の古い `<pr-branch>` が残っている場合は `git rev-list --left-right --count <pr-branch>...origin/<pr-branch>` で同期状況を確認し、ローカル側に独自 commit が無ければ `git reset --hard origin/<pr-branch>` でリモートに揃える。独自 commit がある場合は警告して停止
   - `git branch -d worktree-<worktree-name>` で temp branch を削除

6. **作業ディレクトリ確認**: worktree 内にいることを `git rev-parse --show-toplevel` で確認した上で、後続セクションに進む

### 1.6. PR 番号指定時の branch 確認（`<pr-number>` 指定 かつ `--worktree` 未指定時）

`<pr-number>` を受け取ったが `--worktree` が指定されていない場合、現在の branch が PR の head branch と一致するかを確認する（不一致だと別 branch の差分をレビューする事故が起きるため）。

1. `gh pr view <pr-number> --json headRefName -q .headRefName` で PR の head branch 名を取得
2. `git rev-parse --abbrev-ref HEAD` でカレント branch 名を取得
3. 一致 → そのまま続行
4. 不一致 → エラーで停止し、ユーザーに以下を提示:
   - `--worktree` を付けて worktree 経由でレビューする
   - 手動で `git switch <pr-branch>` してから `/deep-review <pr-number>` を再実行する

### 2. PRコンテキスト確認（自動）
- セクション1でPRからベースブランチを取得できた場合、同時にPR情報も取得:
  1. PRの説明・目的を確認
  2. 関連Issueの自動検出（下記）
  3. 既存のレビューコメント・スレッドを取得（下記GraphQLクエリ）
  4. レビュー後、説明と実際の変更の整合性を評価
- PRが存在しない場合はスキップ

#### 関連Issueの自動検出

PR本文の GitHub Issue Linking キーワード (`Close`/`Closes`/`Closed`/`Fix`/`Fixes`/`Fixed`/`Resolve`/`Resolves`/`Resolved`) に紐づく Issue 参照を検出し、対応する Issue を自動取得する。GitHub 公式の closing keyword 仕様（同リポ `#N` / クロスリポ `OWNER/REPO#N`、大文字小文字・コロン付き許容）に準拠する。

1. `gh pr view --json body --jq '.body'` でPR本文取得
2. 正規表現でリンクキーワード + Issue 参照をマッチ:
   ```
   (?i)\b(close[sd]?|fix(es|ed)?|resolve[sd]?):?\s+(?:([\w.-]+/[\w.-]+)#)?(\d+)
   ```
   - 同リポ形式 (`#N`): キャプチャ3 が空、キャプチャ4 に番号
   - クロスリポ形式 (`OWNER/REPO#N`): キャプチャ3 に `owner/repo`、キャプチャ4 に番号
   - 表記揺れ対応: 大文字 (`CLOSES #10`)、コロン付き (`Closes: #10`)、語頭 word boundary
   - URL 形式 (`https://github.com/owner/repo/issues/N`) は対象外（GitHub の自動 close 対象外で、本文での参照は手動リンク扱いのため、本機能でも揃える）
   - キーワードなしの素の `#N` は対象外（誤検出回避）
3. マッチした Issue 番号それぞれを取得:
   - 同リポ → `gh issue view <N>`
   - クロスリポ → `gh issue view -R <owner>/<repo> <N>`
4. 取得した Issue 情報はセクション6「Issue情報取得時の追加観点」と同じ基準でレビューに利用する

**`--issue` 明示指定との関係**:
- `--issue` が指定されている場合は自動検出をスキップ（ユーザーが特定 Issue を意図的に指定している前提）
- `--issue` 未指定 かつ 自動検出ヒットあり → 検出した Issue 全件を読み込む
- `--issue` 未指定 かつ 自動検出ヒットなし → Issue 観点でのレビューはスキップ

#### 既存レビュースレッド・コメント取得
GraphQL APIで通常コメント・レビュー本文・レビュースレッドを1クエリで取得:
```bash
gh api graphql -f query='{
  repository(owner: "OWNER", name: "REPO") {
    pullRequest(number: PR_NUMBER) {
      comments(first: 50) {
        nodes { author { login } body createdAt url }
      }
      reviews(first: 50) {
        nodes { author { login } state body url }
      }
      reviewThreads(first: 100) {
        nodes {
          isResolved
          isOutdated
          path
          line
          resolvedBy { login }
          comments(first: 20) {
            nodes { author { login } body createdAt url }
          }
        }
      }
    }
  }
}'
```

`comments`（PR本体への通常コメント）と `reviews`/`reviewThreads`（レビュー由来のコメント）は GitHub 上で別物として管理されているため、3つすべてを取得しないと議論経緯を取りこぼす。特に「質問 → 回答 → 合意」「AIレビュー → 対応報告」の流れは PR本体の通常コメントで完結することが多く、これを見落とすと解決済み議題の再提起が発生する。

取得した情報はレビュー実行時に以下のように活用する:
- **解決済みスレッド（`isResolved: true`）**: 既に議論・解決済みのため、同じ内容を指摘しない
- **未解決スレッド（`isResolved: false`）**: 他レビュアーが既に指摘済みのため、同じ内容を重複して指摘しない。ただし、補足や異なる観点がある場合は言及してよい
- **古くなったスレッド（`isOutdated: true`）**: コードが変更されているため、必要に応じて再確認
- **PR本体への通常コメント（`comments[]`）**: PR説明文に書ききれなかった補足、Q&A、対応報告、AIレビュー結果などが入る。レビュー時は議論経緯を把握し、既に解決済みの議題を再提起しないために使う。特に「Xを質問 → Yで回答 → 質問者が合意した」流れは通常コメントで確定するため、見逃すと重複指摘になる。AIレビュー（`[AIによるレビュー]` プレフィックス等）と、それへの対応コメントも同様に考慮する
- **レビュー本文（`reviews[].body`）**: 各レビュアーが Approve/Request Changes/Comment 時に付ける総括コメント。総合的な評価軸を把握するために参照する
- **コメント引用時**: 取得した `url` を併記すると、どのコメントに基づく判断かを明示できる

### 3. コメントモード・個人ルールモード・自動対応モード判定

#### コメントモード
PRにレビューコメントを投稿するかを判定:
1. `--local-only` 指定時 → コメントモードOFF
2. 未指定時 → 自動判定:
   - PRが存在しない場合 → コメントモードOFF
   - PR作成者と現在のユーザーを比較:
     - `gh api user --jq '.login'` で現在のユーザー取得
     - `gh pr view --json author --jq '.author.login'` でPR作成者取得
     - 異なる（他人のPR）→ コメントモードON
     - 同じ（自分のPR）→ コメントモードOFF

自動対応モードONの場合、コメントモードは自動的にOFFになる（自分のPRに自分でレビュー投稿しないため）。

#### 個人ルールモード
個人グローバル `~/.claude/CLAUDE.md` とそのリンク先ドキュメントも準拠チェック対象に含めるかを判定:
- PR作成者が自分 → 個人ルールモードON
- PR作成者が他人 → 個人ルールモードOFF
- PRが存在しない場合 → 個人ルールモードON（自分のコード）

#### 自動対応モード
レビュー指摘事項のうち対応すべきと判断したものを Claude Code が自動で working tree に適用し、コミット・PR更新まで進めるかを判定:
1. `--review-only` 指定時 → 自動対応モードOFF（最優先）
2. 未指定時 → 自動判定:
   - PR作成者が自分 → 自動対応モードON
   - PRが存在しない場合 → 自動対応モードON（自分のコード）
   - PR作成者が他人 → 自動対応モードOFF（行コメント投稿のみ）

判定手段はコメントモードと同じ（`gh api user` と `gh pr view --json author` の比較）。

### 4. 差分取得と確認
- **必ず解析済みのベースブランチを使用** (`git diff <base-branch>...HEAD`)
- 差分が大きい場合は `git diff <base-branch>...HEAD --name-only` で変更ファイル一覧を先に確認
- 各ファイルを `git diff <base-branch>...HEAD -- <ファイル名>` で個別に確認
- 全体を把握してからレビュー評価を実施

**重要原則**:
- 全差分確認: 必ず全ての変更内容を確認してから評価する
- 段階的確認: ファイル一覧 → 個別差分 → 全体評価の順で進める
- 部分的判断禁止: 一部の差分や最初の部分だけを見て結論を出さない
- 見落とし防止: 特に新規追加ファイルや定数・インターフェース変更は必ず確認する
- 指摘の出し切り: 発見した指摘はすべて一度のレビューで出し切る。小出しにして修正→再レビュー→新たな指摘のループを避ける

**広域参照の原則**:
Diff だけで判断せず、変更が周辺コードと相互作用する箇所は能動的に確認する（モダンレビューの差別化要因。Diff-only レビューは変更が他コードと相互作用するバグを見落とす）。

以下に該当する場合は `Grep`/`Read`/`Glob` で差分外のコードも確認する:
- **公開 API/インターフェース/型/関数シグネチャの変更**: 呼び出し元を `Grep` で全て探索し、変更が破壊的でないか確認
- **データ構造・スキーマ・列挙型の変更**: 同じ構造を参照している他箇所が更新漏れになっていないか確認
- **共有モジュール・ユーティリティの変更**: 利用箇所への影響（特に副作用やパフォーマンス特性の変化）を確認
- **テスト未追加の挙動変更**: 既存テストファイルの場所を `Glob` で探し、対応するテストが存在するか、追加・更新されているか確認
- **設定・定数の変更**: 同じ設定キー・定数名を参照している他箇所を確認

ライン引き（過剰探索を避ける）:
- 純粋なリファクタリング（内部実装の整理）で外部から見える挙動が変わらない場合は、広域参照は最小限で良い
- ドキュメント・コメント変更のみの場合は不要
- 「念のため全部 grep する」はオーバーキル。差分内に上記トリガーがあるときに限定する

### 5. Issue情報取得
以下のいずれかに該当する場合に Issue 内容を取得し、要件・仕様を確認する:
- `--issue` 明示指定時 → 指定された Issue 番号を取得
- セクション2「関連Issueの自動検出」で検出された Issue がある時 → 検出された全件を取得

取得は `gh issue view <ISSUE_NUMBER>` で行う。複数 Issue がある場合は順に取得する。

### 6. レビュー実行
以下の観点から詳細にレビュー:

#### レビュー scope（noise-to-signal を上げる）

「重要なことを言う、雑音は出さない」を原則とする。誤検出・nitpick はレビュアーの信頼を損ね、本質的な指摘が埋もれる原因になる。

**原則として指摘しない（他のツールの仕事）**:
- インデント・改行・空白・コーディングスタイルの軽微な揺れ → formatter (prettier, gofmt, black 等) の仕事
- import 順・未使用変数・型注釈漏れ → linter (eslint, golangci-lint, ruff 等) の仕事
- secrets リーク・既知脆弱性パターン → secrets scanner / SAST の仕事
- これらが CI で未検出のままなら「linter/formatter 設定の不備」として PR 説明レベルで1行触れて良いが、各ファイル個別の nitpick として指摘しない

**レビュー対象**:
- バグ・潜在的な問題（実行時エラー、ロジック誤り、競合状態、境界値漏れ）
- セキュリティ上の設計問題（権限管理、入力検証、認証フロー等、SAST が拾わないレイヤー）
- パフォーマンス・スケーラビリティへの影響（明らかなボトルネック、N+1、不必要なネットワーク呼び出し等）
- アーキテクチャ・設計判断（責務分離、結合度、抽象の妥当性）
- ビジネス要件・仕様との整合性
- テストの妥当性（無駄なテスト、抜け漏れ、可読性）

**断定を避ける箇所**:
- アーキテクチャ・ビジネスロジックの妥当性は、レビュアーが前提を見落としている可能性がある領域。断定せず質問形 (`〜という理解で合っていますか？`) で提示する
- シニアエンジニアの判断（アーキテクチャ意図・ドメイン知識）を置き換えるのではなく、観点として提示する

#### 既存レビュー考慮
- [ ] 他レビュアー・自己レビュー・AIレビューが、レビュースレッド・PR本体への通常コメント・レビュー本文のいずれかで既に指摘・議論・解決済みの内容を重複して指摘していないか
- [ ] 未解決の既存指摘と矛盾する指摘をしていないか
- [ ] AIレビュー（`[AIによるレビュー]` プレフィックス等）への対応コメントがある場合、対応後のコード状態を無批判に良いと評価していないか（対応経緯を踏まえて言及する）

#### プロジェクトルール観点
- [ ] リポジトリのCLAUDE.mdに記載されたルール・方針に準拠しているか
- [ ] CLAUDE.mdからリンクされているドキュメント（設計パターン、ワークフロー等）のルールに準拠しているか
  - リンク先ドキュメントのうち、変更内容に関連するものを確認する

#### 個人ルール観点（個人ルールモードON時のみ）
- [ ] 個人グローバル `~/.claude/CLAUDE.md` に記載されたルール・方針に準拠しているか
- [ ] `~/.claude/CLAUDE.md` からリンクされているドキュメント（`~/.claude/context/` 配下）のルールに準拠しているか
  - リンク先ドキュメントのうち、変更内容に関連するものを確認する

#### 基本観点
- [ ] コードの品質（可読性、保守性）
- [ ] バグや潜在的な問題の有無
- [ ] セキュリティリスク（OWASP Top 10等）
- [ ] パフォーマンスへの影響
- [ ] テストコードの妥当性
  - 無駄なテストがないか（過剰なモック、重複テスト等）
  - 必要なテストの抜け漏れがないか（正常系・異常系・エッジケース）
  - テストの可読性（テスト名、構造、意図の明確さ）
- [ ] エラーハンドリングの適切性
- [ ] 命名規則やコーディングスタイルの一貫性

#### 本番運用観点
- [ ] 本番環境でのログ・監視は十分か
- [ ] ロールバック可能な実装か
- [ ] データベースマイグレーションの安全性
- [ ] エッジケースや境界値の考慮
- [ ] 障害時の影響範囲と復旧方法
- [ ] スケーラビリティへの影響

#### Issue情報取得時の追加観点（`--issue` 明示指定 or PR本文から自動検出）
- [ ] Issue要件を満たしているか
- [ ] 仕様から逸脱していないか
- [ ] 関連する要件がすべて実装されているか

#### コメントモードON時の追加要件
各指摘事項について、該当するファイルパスと行番号を特定する。
- 差分内の具体的な行に紐づく指摘 → ファイルパスと行番号を記録（PRコメント投稿時に使用）
- 行番号が特定できない指摘（設計レベルの問題、テスト不足等）→ レビュー本文に含める

#### 自動対応モードON時の追加要件
各指摘事項について、修正適用に必要な情報を確保する。
- 該当するファイルパスと、修正対象のコード範囲（関数・ブロック単位で十分、行番号特定は不要）
- 修正方針（どう書き換えるか）を指摘内容に明記

### 7. レビューコメントの原則と口調

このセクションの原則・口調は、セクション8のローカル出力と、コメントモードON時のPR投稿body・行コメントの両方に適用する。

**原則（指摘の中身）**:
- **コードを主語にする**: 「あなたは〜」ではなく「このコードは〜」。人ではなくコードを対象に書く
- **根拠（なぜ）を添える**: 「こう直す」だけでなく「〜だから〜した方がよい」と理由を示す。意見が割れうる箇所ほど根拠を厚くする
- **重要度を明示する**: 各指摘を出力フォーマットの「必須修正／推奨修正／質問・確認事項」のいずれかに分類し、どの強さの指摘かを曖昧にしない
- **断定しすぎず質問形も使う**: 自分が前提を見落としている可能性がある場合は「〜という理解で合っていますか？」と確認形にする
- **良い点も書く**: 問題点だけでなく、優れた実装は明示的に評価する

**口調（文体）**:
- 中立・簡潔・事実ベースで書く。感情的・誇張的な表現や不要な「！」は避ける
- 断定や指摘で角が立つ箇所は「〜した方がよさそうです」「〜ですかね？」で柔らかくする

### 8. レビュー結果出力
自動対応モードのON/OFFに関わらず、レビュー結果は常に出力する。ユーザーが出力内容を確認し、判断に誤りを感じた場合は Esc で後続の自動対応フローを中断できる。

以下の形式で出力:

```markdown
## レビュー結果

### 総合評価
- Approve可能 / 修正が必要 / 要議論

### 良い点
- ...

### 指摘事項
#### 必須修正
- ...

#### 推奨修正
- ...

#### 質問・確認事項
- ...

### Issue要件の充足状況（Issue情報が取得されている場合のみ）
- ...
```

### コメントモードON時: PRにレビュー投稿

ローカル出力に加えて、PRにレビューコメントを投稿する。

#### 1. PR情報を取得
```bash
gh pr view --json number,headRefOid,url
gh repo view --json owner,name --jq '.owner.login + "/" + .name'
```

#### 2. レビューイベントを決定

| 総合評価 | event |
|---|---|
| Approve可能 | `APPROVE` |
| 修正が必要 | `REQUEST_CHANGES` |
| 要議論 | `COMMENT` |

**判定の注意**:
- 判定は上記の表に機械的に従う。表にない観点（レビュアーとしてのアサイン有無、作成者との関係、遠慮・忖度）で event を変えない
- **質問・確認事項が残っていても APPROVE を COMMENT に格下げしない**。質問を添えて Approve する（LGTM with comments）は Google eng-practices でも推奨される標準的な運用
- APPROVE で質問を添える場合、質問が対応必須でないこと（ブロッキングでないこと）をレビュー本文に明記する
- 「要議論」は「設計判断が割れており、解消しない限り Approve も Request Changes も選べない」ケースに限定する。運用確認レベルの質問は該当しない

#### 3. REST APIでレビューを投稿
```bash
gh api repos/{owner}/{repo}/pulls/{pr_number}/reviews \
  --method POST \
  --input - <<'EOF'
{
  "commit_id": "<HEAD commit SHA>",
  "event": "APPROVE" or "REQUEST_CHANGES" or "COMMENT",
  "body": "## レビュー結果\n\n### 総合評価\n...\n\n### 良い点\n...\n\n### 行に紐づかない指摘\n...",
  "comments": [
    {
      "path": "src/main.go",
      "line": 30,
      "body": "指摘内容"
    }
  ]
}
EOF
```

**パラメータ説明**:
- `commit_id`: PRのHEADコミットSHA（`headRefOid`）
- `event`: レビューイベント（上記の表に従う）
- `body`: レビュー全体のサマリー（総合評価、良い点、行に紐づかない指摘）
- `comments[]`: 行単位の指摘コメント
  - `path`: リポジトリルートからの相対パス
  - `line`: 差分内の行番号（新ファイル側の行番号）
  - `body`: 指摘内容

#### 4. 投稿完了後、PRのレビューURLを表示

### 自動対応モードON時: レビュー指摘の反映

ローカル出力（セクション8）の後、指摘事項を分析して対応すべきものを working tree に適用し、コミット・テスト・PR更新まで進める。

#### 1. 指摘事項の判断
セクション8で出力したすべての指摘（必須修正・推奨修正・質問・確認事項）を対象に、対応すべきかを判断する。

**判断基準**:
- **必須修正**: 原則すべて対応。明らかな誤指摘・前提誤りがある場合のみスキップ
- **推奨修正**: 改善効果と誤指摘リスクを比較し対応可否を判断
- **質問・確認事項**: ユーザーの意図確認が必要なものは対応せず、レビュー出力に残す。コード読解で答えが明確なもののみ対応

**テストコード変更時の留意点**（`~/.claude/context/test-implementation.md` 準拠）:
- 過剰なモック追加は避ける
- テストskipでの誤指摘回避は禁止
- 重複テスト・意味のないテストは追加しない

#### 2. 対応リストの出力
判断結果を以下の形式で出力する:

```markdown
### 自動対応モード: 指摘事項の対応判断

#### 対応する指摘
- <ファイル>: <指摘概要>

#### 対応しない指摘
- <ファイル>: <指摘概要> — <対応しない理由>
```

#### 3. 対応すべきものがゼロの場合
ここで終了。コミット・PR更新は行わない。

#### 4. 対応すべきものがある場合

1. **working tree に修正を適用**
   - 各指摘について Edit / Write ツールで修正を反映
   - 同一ファイルに複数指摘がある場合は副作用を考慮した順序で適用

2. **コミット**
   - @~/.claude/skills/git-commit/SKILL.md に従ってコミット

3. **テスト・Lint実行**
   - プロジェクトのテスト・Lintコマンドを実行（プロジェクトCLAUDE.md依拠）
   - プロジェクトCLAUDE.md等にテスト・Lintコマンドの明示がなければスキップ（推測実行は避ける）
   - 数分以上かかる見込みの場合は `run_in_background: true` で実行
   - **失敗時**: 修正 → コミット → 再テスト を繰り返す（@~/.claude/skills/issue-handle/SKILL.md の実装フェーズ手順4と同じ方針）

4. **プッシュ・PR更新**
   - @~/.claude/skills/git-pr/SKILL.md に従ってプッシュ・PR作成/更新
   - 既存PRがあれば説明更新、未PRなら新規作成

## 注意事項
- ベースブランチが存在しない場合はエラーを通知
- Issue番号が不正な場合はエラーを通知
- ベースブランチの自動判定優先順位: PRのベースブランチ → デフォルトブランチ → `origin/main`（フォールバック）
- `comments` 配列が空の場合（行に紐づく指摘がない場合）でも `body` のみでレビュー投稿は可能
- 自動対応モードはレビュー結果出力後に走るため、ユーザーが出力を確認して Esc で中断できる
- 自動対応モードと既存のコメントモードは排他（自分のPR/未PR時は自動対応、他人のPR時はコメント投稿）
- `--review-only` の用途: サブエージェント経由など、独立セッションでレビューのみ実行したい場合に指定する（修正判断を呼び出し元セッションで行うため）
- **`--worktree` 指定時の挙動**:
  - 並列で別の作業中に呼び出すと、session が PR の worktree に切り替わる。元の作業に戻るには別途 `EnterWorktree(path: <元のworktree>)` を呼ぶ
  - 別ターミナル/別 tmux ペインで `/deep-review <pr-number> --worktree` を実行する運用なら、元 session は触らずに済む（並列レビューの推奨運用）
  - worktree を新規作成する場合、PR の head branch を fetch して checkout するため、PR ブランチ側に未 push のローカル commit があれば事前に push しておくこと
  - **メインリポジトリが PR head branch を checkout 中の場合**: worktree 作成時に自動でメインリポジトリを default branch へ退避する。dirty tree の場合は abort されるため、事前にコミット/stash しておくこと
  - 前提: `worktree.baseRef: "head"` 設定（`~/.claude/settings.json`、dotfiles では設定済み）
- **`<pr-number>` 指定 かつ `--worktree` 未指定時**: カレント branch が PR の head branch と一致しない場合はエラーで停止する（別 branch の差分を誤レビューしないための安全策）
