---
name: understand-pr
description: PR を理解し、目的・変更内容・現状を構造化して報告（<pr-number> で PR を指定、--worktree で対象 PR の worktree に切替）
argument-hint: "[<pr-number>] [--worktree]"
---

# /understand-pr

PR を理解し、作業を引き継げる状態にする

## 使用方法
```
/understand-pr                  # カレント branch の PR（ローカルの状態も含む）
/understand-pr 123              # PR 123（checkout は動かさない）
/understand-pr --worktree       # カレント branch の PR の worktree に切替して報告
/understand-pr 123 --worktree   # PR 123 の worktree に切替して報告
```

## 引数
- `<pr-number>`: 対象 PR 番号（省略時はカレント branch の PR を推論）
- `--worktree`: 対象 PR の worktree に切替（既存があれば再利用、無ければ作成）。並列で別作業中に他の PR を読む際の主用途

## モード

引数で 3 つに分かれる。違うのは worktree 解決を先に行うかと、ローカルの状態を見るかの 2 点だけで、ブリーフの組み立ては共通:

- **番号なし**: カレント branch の PR。ローカルの状態も報告する
- **`<pr-number>` のみ**: checkout を一切動かさない。「現在の状態」のローカル項目は 1 行に置き換わる（同節参照）
- **`--worktree`**（番号の有無を問わず）: worktree 解決を先に行い、その中で番号なしモードと同じブリーフを出す。**本スキルが checkout を動かすのはこの経路だけ**で、ユーザーが worktree を要求したケースに当たる

## 実行内容

以下の各節は、書かれた順に実行するステップ。他の節からはステップ名で参照する（番号を振ると、ステップの挿入で他所の参照が黙って壊れるため）。

### Worktree 解決（`--worktree` 指定時のみ、最初に実行）

Skill ツールで `worktree-resolution` を起動し、その「PR worktree 解決手順」に従って対象 PR の worktree に session を切り替える。

### PR コンテキストの取得

worktree 解決を行った場合はその中で実行する（番号を省略したとき、worktree の branch の PR が解決されるようにするため）:

```bash
ccx pr context <scratchpadディレクトリ> [<pr-number>]
```

出力とコンテキストの読み方は `ccx pr context --help` にある。カレント branch に PR が無い場合は非ゼロ exit + stderr で止まるので、その内容を報告して終了する。

ブリーフの素材（Issue の本文とコメント・PR 本文・コミット・差分・レビュー・待ち状態）はすべてこのドキュメントから取る。ドキュメントが運ばない CI の状態だけを `gh pr checks <pr.number>` で取る（番号はドキュメントの `pr.number` を使い、モードで分岐させない — 番号のみのモードでは checkout が PR を名指さないため）。非ゼロ exit は CI が失敗・保留であることを意味するので、停止せず「残作業・注意点」の材料にする。

### PR 全体の読解

Skill ツールで `pr-reading` を起動し、その手順に従って読む。起動時に名指すのは、container が取得したドキュメントの `linked_issues[]`（読めなかったものを説明する `warnings[]` も同じドキュメントのもの）、文書がそのドキュメント、差分・コミットの取得元も同じドキュメント、報告先が「報告」の「変更内容」。

本スキルが持つのはブリーフへの写像だけで、読む順序・差分の読み切り・生成物の扱い・網羅の確認・読めない Issue・compaction の規則はいずれも同スキルの持ち物:

- **「目的」**: 手順が読んだ意図（Issue とその親の本文・コメント、`pr.body`、`commits[]`）から組み立てる
- **「変更内容」**: 同じく読んだ差分から、ファイル単位ではなく論理的な変更単位で組み立てる。ファイルごとの概要は `diff.files[]`
- 読めなかった Issue と `warnings[]` の各行は「残作業・注意点」に載せる（ブリーフが何を扱えたかを変えるため）

ブリーフは毎回この読解を経て作る。「残作業・注意点」が読む待ち状態は、読解を省く条件ではない。

### 現在の状態の確認
8. `git status` で未コミットの変更を確認
9. `gh pr checks` でCIの状態を確認
10. `gh pr view --json reviewDecision,reviews,headRefOid` でレビュー状態と PR の最新 head を取得
11. `git rev-parse HEAD` を `headRefOid` と比較し、不一致ならローカルが PR の最新 head と乖離していることを記録する（読み取り専用スキルのため fetch・同期はせず、報告への注記のみ）
12. 乖離時、`git cat-file -e <headRefOid>` でオブジェクトがローカルに存在する場合のみ `git merge-base --is-ancestor` で方向（ahead: 未 push commit あり / behind: 未取得 commit あり / diverged）を判別して併記する（push/pull どちらが必要かの引き継ぎ情報になるため。オブジェクト不在なら方向不明のまま報告してよい — fetch はしない）

### 報告
以下の構造で報告する：

```
## 目的
（PRが解決しようとしている課題・背景）

## 変更内容
（主要な変更の要約。ファイル単位ではなく論理的な変更単位で。読解の報告 —
 差分を末尾まで読んだこと・読んだ件数・スキップした生成物 — もここに置く。
 ローカルが PR head より ahead / diverged の場合は、この節が PR の head を
 反映しており手元のコミットではない旨を添える）

## 現在の状態
- ブランチ: xxx
- CI: 成功/失敗/未実行
- レビュー: 承認/変更要求/未レビュー
- 未コミット変更: あり/なし
- ローカルと PR head の整合: 一致 / 乖離（判別できた場合は ahead/behind/diverged を併記。乖離時: コミット履歴・差分の報告はローカル実体基準である旨を明記）

## 残作業・注意点
（レビューコメントの未対応、TODO、既知の問題など）
```

## 注意事項
1. PRの説明文だけでなく、実際の差分を読んで理解する
2. レビューコメントがある場合は未対応のものを特定する
3. CIが失敗している場合は失敗内容も確認する
