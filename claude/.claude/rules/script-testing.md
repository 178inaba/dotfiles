---
paths:
  - "**/go/**"
  - "**/.claude/skills/review-response/SKILL.md"
  - "**/.claude/tests/**"
  - "**/.local/shims/**"
---

# フック・スクリプト編集時のテスト実行

Go モジュール `go/` と、stow 管理下に残るシェル（`~/.local/shims/` 配下。ソースは `shims/.local/shims/`）を編集したら、対応するリグレッションテストを必ず実行する。

理由: フック・スキル配管の失敗モードは silent（見逃し時、実際に事故が起きるまで気付けない）。regression は手動デモでは踏みにくいため、テストでの担保が必須。

## 配置規約と実行

| 編集対象 | テストの場所 | 実行 |
|---|---|---|
| `go/internal/<pkg>/<name>.go` | 同一ディレクトリの `<name>_test.go` | `go -C go test ./...` |
| `shims/.local/shims/<name>` | `shims/.local/shims/tests/test-<name>.sh` | `bash <テストパス>` |
| `claude/.claude/tests/<name>.sh`（`run-all.sh` 等のランナー） | `claude/.claude/tests/test-<name>.sh`（同一ディレクトリ） | `bash <テストパス>` |
| テストファイル自体 | 編集したテストを実行 | |

Go 側はテストを対象コードと同一ディレクトリに置く（Go の慣行そのまま）。シェル側だけが兄弟 `tests/` ディレクトリを使うのは、`shims/` を「全ファイルがランタイム実行対象」に保つため（コロケーションの bash 等価物）。

スキルが使う配管は `ccx` のサブコマンドで、実装は `go/internal/<domain>/`、cobra 定義は `go/internal/cmd/<group>.go`。**1 つのドメインパッケージを複数のスキルが共有する**ので、パッケージを編集したらそのパッケージのテストを走らせれば足りる（シェル時代の「lib を source している全スクリプトのテストも走らせる」列挙手順は、共有が `source` ではなく import になったため不要になった）。

規約から導出できない例外:
- `skills/review-response/SKILL.md`（`<!-- review-response -->` マーカー変更時のみ）: `go -C go test ./internal/pullrequest/`（マーカー同期テスト `TestSkillMarkerMatchesTheSkill`）

シェルスイートの列挙: `find claude/.claude shims -path '*/tests/test-*.sh'`

シェルスイートの一括実行: `bash claude/.claude/tests/run-all.sh claude/.claude shims`（同じパターンで発見して逐次実行し、1つでも失敗したら非ゼロ exit する）。走査ルートを明示するのは、引数を省くとランナーの既定ルート `claude/.claude/` だけになり、`shims/` 側のスイートが silent に漏れるため。**CI（`.github/workflows/ci.yml`）も同じコマンドを実行する**ので、CI の失敗はこのコマンドでそのまま再現できる。

`ccx statusline`（`go/internal/statusline/`）は `claude/.claude/rules/statusline.md` を参照（テスト + 実画面確認が必要なため別ルール）。

## テストの設計制約

- テストは実環境に触れない: 外部コマンドは注入した runner インターフェース、HTTP は `httptest` で差し替える（バイナリにテスト専用の env を残さないため。シェル時代の `GH_BIN`・`YQ_BIN`・`LSOF_BIN` はこれに置き換わって消えた）。git 操作は `t.TempDir()` の使い捨てリポジトリで完結させ、実 gh・実リポジトリに触れない
  - シェル側に残るスイート（`shims/`）は従来どおり env 差し替え（gh は `GH_BIN`）と `mktemp -d` の使い捨てリポジトリを使う
  - 例外として実リポジトリを読むテストが 3 つある（`go/internal/skill` の frontmatter・参照検査は実 `skills/` の検査自体が仕様、`go/internal/pullrequest` のマーカー同期テストは上記の例外）。いずれもネットワーク・認証情報・実 GitHub には触れない
- テストが無いコードを新規追加する場合は、配置規約に従う場所へテストも同時に追加する。規約から導出できない対応になる場合のみ、例外として上記へ追記する
