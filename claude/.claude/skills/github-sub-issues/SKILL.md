---
name: github-sub-issues
description: GitHub Sub-Issues（Issueの親子関係）を作成・リンク・読み取る手順と、親子 Issue の運用規約（葉 Issue = 1 PR、親 = リリース単位、release_manual_steps 節、PR 本文の Part of / Closes）。issue-draft・issue-handle・deep-review が参照する共有知識スキル。gh CLIに専用コマンドがないためAPIを直接使用する
---

# /github-sub-issues

GitHub Sub-Issues機能をgh CLIで操作する方法。専用コマンドが存在しないためAPIを直接使用。

## 基本操作

### Sub-Issue作成とリンク
```bash
# 0. 対象リポジトリを明示（書き込み系は -R 必須）
REPO=$(gh repo view --json nameWithOwner -q .nameWithOwner)

# 1. 親Issue作成（本文は事前に scratchpad へ Write してから --body-file で渡す）
PARENT_ISSUE_URL=$(gh issue create -R "$REPO" --title "親Issue" --body-file parent-body.md --assignee @me)

# 2. Sub-Issue作成
SUB_ISSUE_URL=$(gh issue create -R "$REPO" --title "単体で意味が通るタイトル" --body-file sub-body.md --assignee @me)  # 接頭辞は付けない（「運用規約」）

# 3. APIでリンク
SUB_ISSUE_ID=$(gh api ${SUB_ISSUE_URL/github.com/api.github.com/repos} --jq '.id')
gh api --method POST repos/${REPO}/issues/PARENT_NUMBER/sub_issues \
  --field sub_issue_id=${SUB_ISSUE_ID}

# 4. 依存のリンク（全 Sub の作成後、先行 Sub のマージが必要な Sub にのみ依存順で登録する）
PREREQ_ID=$(gh api ${PREREQ_SUB_URL/github.com/api.github.com/repos} --jq '.id')
gh api --method POST repos/${REPO}/issues/SUB_NUMBER/dependencies/blocked_by \
  --field issue_id=${PREREQ_ID}
```

手順 3 は既に存在する Issue にもそのまま使える（Sub-Issue 化は作成とは独立の操作のため）。作成済みの Issue を親にぶら下げるだけなら手順 1〜2 を飛ばし、下記の `link_sub_issue` を使う。

### ワンライナー関数
```bash
create_sub_issue() {
  local parent=$1 title=$2 body_file=$3
  local repo=$(gh repo view --json nameWithOwner -q .nameWithOwner)
  local sub_url=$(gh issue create -R "$repo" --title "$title" --body-file "$body_file" --assignee @me)
  local sub_id=$(gh api ${sub_url/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/${repo}/issues/$parent/sub_issues \
    --field sub_issue_id=$sub_id
  echo "Created: $sub_url"
}

link_sub_issue() {
  # link_sub_issue <親 Issue の番号> <既存 Issue の URL>（作成はせずリンクだけ行う）
  local parent=$1 sub_url=$2
  local repo=$(gh repo view --json nameWithOwner -q .nameWithOwner)
  local sub_id=$(gh api ${sub_url/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/${repo}/issues/$parent/sub_issues \
    --field sub_issue_id=$sub_id
}

link_blocked_by() {
  # link_blocked_by <blocked Issue の番号> <先行 Issue の URL>
  local blocked=$1 prereq_url=$2
  local repo=$(gh repo view --json nameWithOwner -q .nameWithOwner)
  local prereq_id=$(gh api ${prereq_url/github.com/api.github.com/repos} --jq '.id')
  gh api --method POST repos/${repo}/issues/$blocked/dependencies/blocked_by \
    --field issue_id=$prereq_id
}
```

## 親子関係の読み取り

親の有無・Sub 一覧・完了状態・依存を読むときは `ccx issue tree <issue-number> [-R owner/repo] [--with-prs] [--with-deps]` を使う（親は専用エンドポイントの 404 が「親なし」、Sub 一覧と blocker 一覧はページネーション付きで、都度組み立てると扱いがぶれるため。`--with-prs` は各 Sub を閉じた PR の状態・マージ先を、`--with-deps` は各 Sub の blocker を付ける）。出力の読み方は `ccx issue tree --help` にある。

## 本文の節の読み取り

Issue 本文の節は**意味キー**で識別する。キー ↔ 見出しの対応と「なし」の固定文字列を持つのは `ccx issue sections` だけで、**この SKILL.md に表を複製しない**（複製はドリフトの元。起案側の使い方は issue-draft の「本文の節」）。消費側の規約:

- 節は `ccx issue sections find <本文を書き出したファイル> <key>` で引く（本文は `gh issue view <番号> -R <owner>/<repo> --json body -q .body` でファイルへ落としてから渡す）。`find` は ja・en 両方の canonical 見出しを受け付けるので、消費側はロケールを知らなくてよい
- **節が見つからないときの扱いは消費側が決める**（キーごとに違う — `release_manual_steps` は推定した上でユーザーに確認、`composition` は順序の制約なしとして扱う）。ja / en どちらの見出しでもない本文もここに落ちる。取得に失敗した入力は「節なし」ではなく前提不成立として弾かれるので、この分岐には入らない
- 「なし」マーカーは `ccx issue sections schema <key>` の `none_markers` から取り、**ja / en 両方を受け付ける**。照合は緩く見る — 節本文がマーカーと同じ「なし」を述べていればよく、後続の句読点・補足文はそれを覆さない。作業項目が 1 つでも挙がっていれば「なし」ではない

出力と exit status の読み方は `ccx issue sections find --help` と `ccx issue sections schema --help` にある。

## 運用規約（親子 Issue と PR の対応）

issue-draft（起票）・issue-handle（実装）・deep-review（レビュー）が共有する規約。定義はここが正で、各スキルは自分が実行する手順だけを書く。

- **葉 Issue = 1 PR = 1 実装セッション**。リリース単位（一緒に出さないと価値が完結しない範囲）が 1 PR に収まらないときは、親 Issue（リリース単位）+ Sub-Issues（各 1 PR）に分ける。進捗の正は Issue 階層の open/closed であり、本文のチェックリストやスキル側の状態管理で別管理しない。1 Issue に複数 PR を示唆する散文（「PR 分割の目安」等）は書かない — issue-handle は 1 Issue を 1 PR で処理し `Closes #<Issue>` を書くため消費できず、最初の PR のマージで Issue が閉じる
- **仕様の配置（重複禁止）**: 全体に効くもの（横断ルール・エラーコード等の確定値・トランザクション順序・共通の参照文書等）は親にだけ、段階固有のスコープ・受け入れ条件は Sub にだけ書く。Sub の自己完結基準は「**親と合わせて読めば迷わず実装できる**」。したがって**葉を読むスキルは親の本文とコメントも取得**し、親の横断ルール・確定事項を葉の要件と同格に扱う。ただし要件充足の判定対象は葉自身の受け入れ条件のみ（親の受け入れ条件は他の Sub にまたがる）
- **Sub の必須条件**: 単独でベースブランチにマージしても壊れない（未配線のルート・未スケジュールのバッチ・未参照のテーブル等は可）。リリースを揃える責任は親に置く
- **親の `release_manual_steps` 節**（固定なのはキーで、見出し文字列はロケールごとに `ccx issue sections` の表が持つ。issue-draft が親の必須節としてレンダリングする）: `none_markers` の「なし」マーカーまたはチェックリスト。マーカーなら最後の Sub の PR で親を閉じてよく、作業ありなら親は作業完了後に手動で閉じる。節が見つからない親（他の経路で立てられた Issue 等。引き方は「本文の節の読み取り」）は本文・コメントから推定した上でユーザーに確認し、推測で埋めない
- **PR 本文**: Sub の PR は `Closes #<Sub>` に加えて `Part of #<親>`（closing keyword ではないので親は閉じず、親の Development サイドバーに全 Sub の PR が並ぶ。親が別リポジトリ — `ccx issue tree` の `parent.same_repo: false` — なら `Part of owner/repo#N` 形式で書く）。PR 作成時点で他の全 Sub が closed（`ccx issue tree` の `all_siblings_closed: true`）かつ `release_manual_steps` が「なし」マーカーなら `Closes #<親>` も書く。並列で複数の Sub が open のうちはどの PR も親を閉じず、全 Sub 完了後に親を issue-handle に渡して充足検証 → close する
- **Sub のタイトル**: 接頭辞（【Sub】・[1/6] 等）を付けず単体で意味が通るものにする。親子関係は GitHub の Sub-Issues 表示が担い、後から Sub を足すと番号がずれる
- **Sub 間の順序**: GitHub ネイティブの `blocked_by` リンク（上記「Sub-Issue作成とリンク」手順 4）が正で、`ccx issue tree` の `blocked_by[]` / `blockers_closed` から読む。散文（親の `composition` 節の一覧・Sub の `depends_on` 節）は人が読むために残すが、判定の根拠にはしない。**例外**は依存が 1 件も登録されていない Issue（本規約より前に立てた Issue・他の経路で立てられた親）で、この場合のみ散文へフォールバックする。散文だけを正にしないのは、文言がぶれる・後から足した Sub が親の一覧から漏れるといった読み取り事故が起きる上、スキルの外（GitHub の UI・Projects）から依存が見えないため

## リンク後の親Issue本文同期

Sub-Issueのリンク完了後、親Issue本文の子Issueへの言及を新しいSub-Issueを含む状態に同期する。

1. 親Issue本文を取得: `gh issue view PARENT_NUMBER -R "$REPO" --json body -q .body`
2. 子Issueを列挙している箇所（タスクリスト・箇条書き・表等）があるか確認
3. **あれば**: 同じ形式・同じ粒度で新しいSub-Issueを追記し、本文全体をファイルに書き出して `gh issue edit PARENT_NUMBER -R "$REPO" --body-file <path>` で更新
4. **無ければ**: 本文は変更しない（GitHubがSub-Issuesパネルを標準表示するため本文リストの新設は不要。既存の書き方を尊重する）

## 注意事項
- 整数型の`id`を使用（`node_id`ではない）。`sub_issue_id`・`blocked_by` の `issue_id` とも同じ
- リポジトリ書き込み権限が必要
- `gh issue create` 等の書き込み系は `-R owner/repo` でリポジトリを明示し、複数行本文は `--body-file` で渡す（グローバル方針。いずれも gh shim が強制。`--body` に `\n` を含む文字列を直渡しすると、bash はリテラルの `\n` として本文に焼き込む）
- [GitHub REST API Docs: Sub-issues](https://docs.github.com/en/rest/issues/sub-issues) / [Issue dependencies](https://docs.github.com/en/rest/issues/dependencies)
