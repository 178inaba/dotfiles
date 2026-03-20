---
name: web-search
description: WebSearchツールが利用不可の場合にPlaywright CLIでWeb検索を代替実行
argument-hint: <検索クエリ>
disable-model-invocation: false
---

# /web-search

WebSearchツールが利用できない環境でのWeb検索代替手段。
Playwright CLI（Headedモード）でブラウザ検索を実行する。

## 使用方法
```
/web-search <検索クエリ>
```

## 前提条件
- WebSearchツールが利用不可（deferred toolsに含まれない、ToolSearchで見つからない）

## 実行内容

### 1. 検索実行
```bash
PLAYWRIGHT_MCP_OUTPUT_DIR=/tmp/playwright-cli-output npx @playwright/cli open --headed "https://www.google.com/search?q=<URLエンコード済みクエリ>"
```

### 2. 検索結果の読み取り
コマンド出力に含まれるスナップショットファイルパスを確認し、そのファイルを読み取って検索結果を抽出・要約する。

## 注意事項
- Google検索でCAPTCHAが表示された場合、DuckDuckGo（`https://duckduckgo.com/?q=<クエリ>`）にフォールバック
