---
paths:
  - "**/.claude/statusline.sh"
---

# statusline 編集ルール

`~/.claude/statusline.sh`（ソース: `claude/.claude/statusline.sh`）を編集する際のルール。

## 表示項目の追加前に重複を確認する

stdin JSON にフィールドが存在することは表示する価値があることを意味しない。追加前に以下の両方と重複しないか**実画面で**確認する:

1. **Claude Code 組み込みフッター**: statusline はフッターを置き換えず並行表示され、フッターは非表示にできない
   - PR バッジ（クリック可能・レビュー状態の色ドット付き。statusline JSON の `pr.*` はこのバッジのミラー）
   - effort レベルバッジ、mode 表示等
2. **statusline 自身の既存表示**: 1行目のパス（worktree 名はパス末尾に常に出る）、2行目のブランチ名等
   - 文字列比較で重複回避する場合は命名規約による変形も考慮する（例: worktree 名は branch 名の `/`→`-` 置換なので単純比較では常に「異なる」と判定される）

過去の経緯: PR 番号表示と worktree 名表示を追加したが、上記の重複により両方削除した（コミット 429b731, cdc9656）。

## 検証方法

サンプル JSON を stdin に流して確認する:

```bash
bash claude/.claude/statusline.sh <<'EOF'
{"session_id":"test","workspace":{"current_dir":"/path","project_dir":"/path"},"model":{"display_name":"Opus"},"cost":{"total_cost_usd":1.23,"total_duration_ms":5400000},"context_window":{"used_percentage":42.5},"rate_limits":{"five_hour":{"used_percentage":35,"resets_at":1751600000},"seven_day":{"used_percentage":73,"resets_at":1751900000}}}
EOF
```

最低限のケース: 全フィールドあり / 対象フィールドなし / 空 stdin（フォールバック動作）。`bash -n` で構文チェックも行う。

## 設定の整合性

キャッシュ関連の制約（TTL と `statusLine.refreshInterval` の同期、作業ディレクトリ単位のキー設計）は statusline.sh 内の該当コメントを正とする。この文書には重複記載しない（乖離防止のため）。
