---
paths:
  - "**/.claude/hooks/**"
  - "**/.claude/scripts/**"
  - "**/.claude/skills/**/scripts/**"
  - "**/.claude/skills/review-response/SKILL.md"
  - "**/.claude/tests/**"
---

# フック・スクリプト編集時のテスト実行

`~/.claude/` 配下のフック・スクリプト（ソース: `claude/.claude/`）を編集したら、対応するリグレッションテストを必ず実行する。

理由: フック・スキルスクリプトの失敗モードは silent（見逃し時、実際に事故が起きるまで気付けない）。regression は手動デモでは踏みにくいため、テストでの担保が必須。

## 配置規約と実行

テストは**対象コンポーネントの隣**の `tests/` ディレクトリに置き、テストの場所は規約から導出する（実行はすべて `bash <テストパス>`）。対象と同一ディレクトリに混ぜないのは、`hooks/`・`scripts/` を「全ファイルがランタイム実行対象」に保つため（Go のコロケーションの bash 等価物として兄弟ディレクトリを使う）:

| 編集対象 | テストの場所 |
|---|---|
| `hooks/<name>.sh` | `hooks/tests/test-<name>.sh` |
| `scripts/<name>.sh`（スキル横断の共有スクリプト） | `scripts/tests/test-<name>.sh` |
| `skills/<skill>/scripts/<name>.sh` | `skills/<skill>/tests/test-<name>.sh` |
| ルート直下のスクリプト（`statusline.sh` 等） | `tests/test-<name>.sh` |
| テストファイル自体 | 編集したテストを実行 |

規約から導出できない例外:
- `hooks/start-caffeinate.sh`・`hooks/stop-caffeinate.sh`: ペアで `hooks/tests/test-caffeinate.sh`
- `skills/review-response/SKILL.md`（`<!-- review-response -->` マーカー変更時のみ）: `scripts/tests/test-fetch-pr-context.sh`（マーカー同期テスト）

全テストの列挙: `find claude/.claude -path '*/tests/test-*.sh'`

`statusline.sh` は `claude/.claude/rules/statusline.md` を参照（テスト + 実画面確認が必要なため別ルール）。

## テストの設計制約

- テストは実環境に触れない: 外部コマンドは env 差し替えでスタブ化する（caffeinate は `CAFFEINATE_BIN`、gh は `GH_BIN`）。git 操作は `mktemp -d` の使い捨てリポジトリで完結させ、実 gh・実リポジトリに触れない。テスト・スクリプトの変更でこの性質を壊さない
- テストが無いフック・スクリプトを新規追加する場合は、配置規約に従う場所へテストも同時に追加する。規約から導出できない対応になる場合のみ、例外として上記へ追記する
