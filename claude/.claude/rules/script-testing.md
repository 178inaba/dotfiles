---
paths:
  - "**/go/**"
  - "**/.claude/hooks/**"
  - "**/.claude/scripts/**"
  - "**/.claude/skills/**/scripts/**"
  - "**/.claude/skills/review-response/SKILL.md"
  - "**/.claude/tests/**"
  - "**/.local/shims/**"
---

# フック・スクリプト編集時のテスト実行

stow 管理下のフック・スクリプト（`~/.claude/` 配下と `~/.local/shims/` 配下。ソースはそれぞれ `claude/.claude/`・`shims/.local/shims/`）と Go モジュール `go/` を編集したら、対応するリグレッションテストを必ず実行する。

Go モジュール側は本文書の配置規約の対象外で、テストは `_test.go` として対象コードと同一ディレクトリに置く（下記の兄弟 `tests/` ディレクトリは、そのコロケーションを bash で再現したもの）。実行は `go -C go test ./...`。

理由: フック・スキルスクリプトの失敗モードは silent（見逃し時、実際に事故が起きるまで気付けない）。regression は手動デモでは踏みにくいため、テストでの担保が必須。

## 配置規約と実行

テストは**対象コンポーネントの隣**の `tests/` ディレクトリに置き、テストの場所は規約から導出する（実行はすべて `bash <テストパス>`）。表の「編集対象」は最初の行だけ repo ルート相対で、残りは `claude/.claude/` 相対。対象と同一ディレクトリに混ぜないのは、`hooks/`・`scripts/` を「全ファイルがランタイム実行対象」に保つため（Go のコロケーションの bash 等価物として兄弟ディレクトリを使う）:

| 編集対象 | テストの場所 |
|---|---|
| `shims/.local/shims/<name>` | `shims/.local/shims/tests/test-<name>.sh` |
| `hooks/<name>.sh` | `hooks/tests/test-<name>.sh` |
| `scripts/<name>.sh`（スキル横断の共有スクリプト） | `scripts/tests/test-<name>.sh` |
| `skills/<skill>/scripts/<name>.sh` | `skills/<skill>/tests/test-<name>.sh` |
| ルート直下のスクリプト | `tests/test-<name>.sh` |
| `tests/<name>.sh`（`run-all.sh` 等のランナー） | `tests/test-<name>.sh`（同一ディレクトリ） |
| テストファイル自体 | 編集したテストを実行 |

source 用の共有 lib（`*-lib.sh`。`scripts/` と `skills/<skill>/scripts/` 配下）を編集したときは、上の表に加えて次の2つを実行する:

1. その lib の単体テスト（表の場所にあれば。持たない lib もある）
2. その lib を source している全スクリプトのテスト（下記で source 元を列挙し、各テストの場所を表から導出する）

2 が要るのは、lib の挙動が source 元の観測可能な出力（stderr 文言・stdout の JSON・exit code）に直結しており、lib だけを編集すると破損が source 元側に潜伏するため。

source 元の列挙（repo root で実行。`<lib>` は `warnings-lib` のような拡張子抜きの lib 名）:

```bash
grep -rlE '^(\.|source)[[:space:]]+.*<lib>\.sh' claude/.claude --include='*.sh'
```

行頭の source 行だけを拾うのは、lib 名で検索するとヘッダーコメントでの言及まで一致するため（`worktreeinclude-lib.sh`・`sync-lib.sh` は共に `warnings-lib.sh` をコメントで参照している）。この列挙が成立する前提として、**caller の source 行は行頭・非インデントで書き、lib 名はリテラルで書く**（パスの前半が変数なのは可）。`. "$LIB"` のように lib 名まで変数に入れると列挙から silent に漏れる。lib 自身の単体テストはこの制約の対象外（変数経由でよい）。

もう1つの前提として、**lib は他の lib を source しない**（必要な関数は呼び出し元が両方を source して渡す）。`worktreeinclude-lib.sh`・`sync-lib.sh` のヘッダーがこの契約を定めており、新しい lib もこれに従う。破ると transitive な caller が1段の grep から漏れるため、この前提が保たれる限り上記の列挙で尽きる。

規約から導出できない例外:
- `hooks/start-caffeinate.sh`・`hooks/stop-caffeinate.sh`: ペアで `hooks/tests/test-caffeinate.sh`
- `skills/review-response/SKILL.md`（`<!-- review-response -->` マーカー変更時のみ）: `scripts/tests/test-fetch-pr-context.sh`（マーカー同期テスト）

全テストの列挙: `find claude/.claude shims -path '*/tests/test-*.sh'`

全テストの一括実行: `bash claude/.claude/tests/run-all.sh claude/.claude shims`（同じパターンで発見して逐次実行し、1つでも失敗したら非ゼロ exit する）。走査ルートを明示するのは、引数を省くとランナーの既定ルート `claude/.claude/` だけになり、`shims/` 側のスイートが silent に漏れるため。共有 lib のように影響範囲が広い編集では、上の表から個別に導出するより先にこれを回す方が速い。**CI（`.github/workflows/ci.yml`）も同じコマンドを実行する**ので、CI の失敗はこのコマンドでそのまま再現できる。

`ccx statusline`（`go/internal/statusline/`）は `claude/.claude/rules/statusline.md` を参照（テスト + 実画面確認が必要なため別ルール）。

## テストの設計制約

- テストは実環境に触れない: 外部コマンドは env 差し替えでスタブ化する（caffeinate は `CAFFEINATE_BIN`、gh は `GH_BIN`）。git 操作は `mktemp -d` の使い捨てリポジトリで完結させ、実 gh・実リポジトリに触れない。テスト・スクリプトの変更でこの性質を壊さない
  - **Go モジュールでは env 差し替えを使わない**: 外部コマンドは注入した runner インターフェース、HTTP は `httptest` で差し替える（バイナリにテスト専用の env を残さないため）。使い捨てリポジトリで実 git を叩くのは同じ
- テストが無いフック・スクリプトを新規追加する場合は、配置規約に従う場所へテストも同時に追加する。規約から導出できない対応になる場合のみ、例外として上記へ追記する
