---
paths:
  - "**/go/**"
  - "**/.claude/skills/review-response/SKILL.md"
---

# Go モジュール編集時のテスト実行

Go モジュール `go/` を編集したら、対応するリグレッションテストを必ず実行する。

理由: フック・スキル配管の失敗モードは silent（見逃し時、実際に事故が起きるまで気付けない）。regression は手動デモでは踏みにくいため、テストでの担保が必須。

## 配置規約と実行

| 編集対象 | テストの場所 | 実行 |
|---|---|---|
| `go/internal/<pkg>/<name>.go` | 同一ディレクトリの `<name>_test.go` | `go -C go test ./...` |
| `go/cmd/<name>/main.go` | 実装は `go/internal/` 側にあるので、そのパッケージのテスト | 同上 |
| テストファイル自体 | 編集したテストを実行 | |

テストは対象コードと同一ディレクトリに置く（Go の慣行そのまま）。

スキルが使う配管は `ccx` のサブコマンドで、実装は `go/internal/<domain>/`、cobra 定義は `go/internal/cmd/<group>.go`。**契約に関わる型・json タグ・doc comment を触ったら `go -C go generate ./internal/contract/...` を実行する**（`docs_gen.go` と `testdata/*.txt` はどちらも同じ doc comment・json タグの射影で、この 1 コマンドが両方を書き直す。古いままだと `go/internal/contract` の `TestGenerated` が落ちる）。**1 つのドメインパッケージを複数のスキルが共有する**ので、パッケージを編集したらそのパッケージのテストを走らせれば足りる。

規約から導出できない例外:
- `skills/review-response/SKILL.md`（`<!-- review-response -->` マーカー変更時のみ）: `go -C go test ./internal/pullrequest/`（マーカー同期テスト `TestSkillMarkerMatchesTheSkill`）

**CI（`.github/workflows/ci.yml`）は `go vet`・golangci-lint・`go test -race` の 3 つだけを実行する**ので、CI の失敗はローカルで再現できる。**lint も push 前に回す**: `cd go && golangci-lint run`。`go test` が通っても `revive` の exported ルール等は落ちるので、テストだけでは代替にならない。バージョンは `go/.tool-versions` のピンを mise が解決するので、新しいマシンでは `mise install`（`go/` で実行。または `mise -C go install`）で取得する。

`ccx statusline`（`go/internal/statusline/`）は `claude/.claude/rules/statusline.md` を参照（テスト + 実画面確認が必要なため別ルール）。

## テストの設計制約

- テストは実環境に触れない: 外部コマンドは注入した runner インターフェース、HTTP は `httptest` で差し替える（バイナリにテスト専用の env を残さないため。シェル時代の `YQ_BIN`・`LSOF_BIN` 等はこれに置き換わって消えた）。git 操作は `t.TempDir()` の使い捨てリポジトリで完結させ、実 gh・実リポジトリに触れない
  - **`GH_BIN` だけは残る**（`go/internal/ghshim`）。実体の `gh` を探すこと自体が gh shim の仕事なので、これはテスト用の穴ではなく本番の入力
  - ファイルシステムは runner 注入の対象外で、`t.TempDir()` に実際のツリーを組んで検証してよい（`go/internal/selfbuild` の symlink 解決、`go/internal/ghshim` の本文ファイルがこの形）
  - 例外として実リポジトリを読むテストが 3 つある（`go/internal/skill` の frontmatter 検査と `go/internal/cmd` の参照検査は実 `skills/` の検査自体が仕様、`go/internal/pullrequest` のマーカー同期テストは上記の例外）。いずれもネットワーク・認証情報・実 GitHub には触れない
  - 参照検査が `go/internal/cmd` にあるのは、突き合わせる契約識別子を組み立てられるのがそこだけだから（どのコマンドがどの型を描画するかを知っているのは `internal/cmd`）
- 外部コマンドを実際に起動するテストは、**production がそのコマンドを叩く経路の検証**に限る（`gittest` の実 git、`go/internal/worktree` の実 `lsof`）。テスト都合のビルドやスタブスクリプトを起こす形は採らない
- テストが無いコードを新規追加する場合は、配置規約に従う場所へテストも同時に追加する。規約から導出できない対応になる場合のみ、例外として上記へ追記する
