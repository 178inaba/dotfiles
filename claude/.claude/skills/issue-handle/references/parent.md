# 親 Issue を渡されたとき

`ccx issue tree` の `kind` が `parent` / `parent_and_sub`（Sub あり）の実行が読む。この Issue は実装対象ではなく（Sub が実装単位）、計画フェーズ・実装フェーズには進まない。

## open の Sub が残る場合

`all_sub_issues_closed: false` → **停止**。`ccx issue tree <parent> --with-deps` で各 Sub の blocker を取り、Sub 一覧を番号・タイトル・状態で提示し、「次に着手できる Sub」を示して終了する。自動では着手しない（どの Sub をやるか・`--worktree` を使うかはユーザーの判断）

- 対象は **open の Sub のみ**（closed の Sub は blocker がすべて closed でも着手可に含めない）。その上で Sub ごとに判定して 1 つの一覧にまとめる: `blocked_by` が空でない Sub は `blockers_closed: true` なら着手可、`blocked_by` が空の Sub は運用規約「Sub 間の順序」の例外（散文へのフォールバック）で判定する

## 親の充足検証 → close

`all_sub_issues_closed: true`（全 Sub 完了の親を渡されたとき）:

1. 事前準備 Step 1〜2 と同じ規則でベースブランチを確定し（`--base` / 現在ブランチ）、`origin/<base>` を fetch する
2. `ccx issue tree <parent> --with-prs` で各 Sub を閉じた PR の状態を取り（`sub_issues[].prs[]` の `merged` / `base_ref`）、未マージ・`base_ref` がベースブランチと異なる・`prs` が空の Sub があれば警告し、続行するか AskUserQuestion で確認する
3. 親本文の受け入れ条件・横断ルール（と Sub の受け入れ条件のうち親に集約されているもの）を項目展開し、`origin/<base>` のコードと突き合わせて **充足 / 未実装 / 逸脱** に分類する（deep-review の「Issue 要件の充足状況」と同じ形式。差分ではなくベースブランチの現状を読む）
4. 未実装・逸脱が 1 つでもあれば close せず、充足表と未充足の内容を報告して終了する（対応は新しい Sub の起票等、ユーザーの判断）
5. 全充足なら `release_manual_steps` 節を確認する（`github-sub-issues` の「本文の節の読み取り」で引く）。「なし」マーカー（または節が無く手動作業も見当たらない）なら充足表を提示して close の承認を得てから閉じる: 充足表を scratchpad に Write して `gh issue comment <parent> -R <repo> --body-file <path>` で投稿（言語は Issue 本文に合わせる）→ `gh issue close <parent> -R <repo>`。手動作業ありなら、作業の完了をユーザーに確認できた場合のみ同じ手順で close し、未完了なら close せず作業一覧を提示して終了する
