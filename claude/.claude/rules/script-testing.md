---
paths:
  - "**/.claude/hooks/**"
  - "**/.claude/scripts/**"
  - "**/.claude/skills/**/scripts/**"
  - "**/.claude/tests/**"
---

# フック・スクリプト編集時のテスト実行

`~/.claude/` 配下のフック・スクリプト（ソース: `claude/.claude/`）を編集したら、対応するリグレッションテストを必ず実行する。

理由: フック・スキルスクリプトの失敗モードは silent（見逃し時、実際に事故が起きるまで気付けない）。regression は手動デモでは踏みにくいため、テストでの担保が必須。

## 対応表

| 編集対象 | テストコマンド |
|---|---|
| `hooks/gh-write-guard.sh` | `bash claude/.claude/hooks/tests/test-gh-write-guard.sh` |
| `hooks/worktree-edit-guard.sh` | `bash claude/.claude/hooks/tests/test-worktree-edit-guard.sh` |
| `hooks/start-caffeinate.sh`・`hooks/stop-caffeinate.sh` | `bash claude/.claude/hooks/tests/test-caffeinate.sh` |
| `scripts/fetch-pr-context.sh` | `bash claude/.claude/tests/test-fetch-pr-context.sh` |
| `skills/cleanup-merged/scripts/collect-candidates.sh` | `bash claude/.claude/tests/test-collect-candidates.sh` |
| `skills/review-assigned-prs/scripts/list-pending-reviews.sh` | `bash claude/.claude/tests/test-list-pending-reviews.sh` |
| `skills/review-assigned-prs/scripts/ensure-clone.sh` | `bash claude/.claude/tests/test-ensure-clone.sh` |
| `skills/review-assigned-prs/scripts/verify-posted-reviews.sh` | `bash claude/.claude/tests/test-verify-posted-reviews.sh` |
| `hooks/slack-notify.sh` | `bash claude/.claude/hooks/tests/test-slack-notify.sh` |
| `hooks/subagent-tracker.sh` | `bash claude/.claude/hooks/tests/test-subagent-tracker.sh` |
| `hooks/idle-notify.sh` | `bash claude/.claude/hooks/tests/test-idle-notify.sh` |
| テストファイル自体 | 編集したテストを実行 |

`statusline.sh` は `claude/.claude/rules/statusline.md` を参照（テスト + 実画面確認が必要なため別ルール）。

## テストの設計制約

- テストは実環境に触れない: 外部コマンドは env 差し替えでスタブ化する（caffeinate は `CAFFEINATE_BIN`、gh は `GH_BIN`）。git 操作は `mktemp -d` の使い捨てリポジトリで完結させ、実 gh・実リポジトリに触れない。テスト・スクリプトの変更でこの性質を壊さない
- テストが無いフック・スクリプトを新規追加する場合は、テストと上記対応表の行も同時に追加する
