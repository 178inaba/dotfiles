---
name: git-rebase
description: PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消して `--force-with-lease` でプッシュ
disable-model-invocation: true
---

# /git-rebase

PRのベースブランチへrebaseし、コンフリクトが発生したら自動解消して `--force-with-lease` でプッシュ

## 使用方法
```
/git-rebase
```

## 実行内容

### 1. PR文脈の取得
Skill ツールで `understand-pr` を起動し、PRの目的・変更内容・現状を把握する。

PR文脈はコンフリクト解消時の判断材料として使用。

### 2. ベースブランチの決定
1. `gh pr view --json baseRefName --jq '.baseRefName'` でPRのベースブランチを取得
2. PRが存在しない場合は `gh repo view --json defaultBranchRef --jq '.defaultBranchRef.name'` でデフォルトブランチを取得
3. 取得したブランチ名を `origin/[branch-name]` 形式で使用

### 3. リモート最新化
4. `git fetch origin [branch-name]` でベースブランチを最新化

### 4. rebase実行
5. `git status` で未コミット変更がないことを確認（あればユーザーに確認）
6. `git rebase origin/[branch-name]` を実行
7. コンフリクトなく完了した場合は完了確認とプッシュへ

### 5. コンフリクト自動解消
コンフリクトが発生した場合、以下を繰り返す：

8. `git status` でコンフリクトファイルを特定
9. 各コンフリクトファイルについて：
    - ファイルを読んでコンフリクトマーカー（`<<<<<<<`, `=======`, `>>>>>>>`）を確認
    - PR文脈と両側の変更意図を踏まえて解消
    - `git log --oneline origin/[branch-name] -- [file]` でmain側の変更履歴を確認（必要に応じて）
    - `git log --oneline HEAD -- [file]` でPR側の変更履歴を確認（必要に応じて）
10. 解消したファイルを `git add [file]` でステージング
11. `git rebase --continue` で次のコミットへ進む
12. さらにコンフリクトが発生したら 8 から繰り返す

### 6. 完了確認とプッシュ
13. `git status` でクリーンな状態を確認
14. `git log --oneline -10` でrebase後の履歴を確認
15. `git rev-parse --abbrev-ref @{u}` でupstreamの有無を確認（非ゼロexit = upstreamなし）
    - **upstreamなし**（一度もpushしていない・PRが無い）: プッシュしない。rebase結果を報告し、初回pushを行う `/git-pr` を案内して終了する
    - この判定が成り立つのは `git/.gitconfig` の `branch.autoSetupMerge = simple` が、`origin/main` のような異なる名前の始点から作ったブランチに upstream を引き継がせないため（引き継ぐと一度もpushしていなくても `@{u}` が解決してしまう）
16. `git push --force-with-lease` でプッシュ（**この引数形そのまま**。何も付け足さない）
    - **プッシュ前のゲート**: ステップ1で得たローカルHEADとPRの最新head（`head_oid`）の整合が **behind / diverged** だった場合はプッシュしない。乖離の内容を報告して停止する（rebase開始時点でリモートに未取得のcommitがあったということで、注意事項4のとおりleaseでは止まらない）。PRが無く整合を判定できない場合はゲートを適用せずleaseに委ねる
    - 形を固定する理由: 許可ルール `Bash(git push --force-with-lease)` は完全一致で、prefixルールにすると `--force` の併記・`+<refspec>`・`=<ref>:<sha>` がleaseを無効化してリモートを上書きできてしまうため（3形とも実測で `forced update` になったためprefixルールは不採用）
    - **`stale info` で拒否された**（rebase中にリモートのbranchが動いた）: リトライしない。fetchし直しての再rebaseもしない。拒否出力を添えてユーザーに報告して停止する（相手側の変更を取り込むかはユーザーの判断）
    - **その他の失敗**（認証・ネットワーク・保護ブランチ等）: リトライせず、出力をそのままユーザーに報告して停止する
17. ユーザーに結果（rebase内容とプッシュ結果）を報告

## 自動解消の判断基準

基本的に自動解消するが、以下のケースは判断が困難なため、ユーザーに確認する：

- **同じ機能の別実装**: PR側で実装した機能が、main側で別アプローチで実装済み
- **大規模リファクタリング**: main側でPRの前提が崩れる構造変更
- **削除 vs 変更**: 一方がファイル/関数を削除、もう一方が変更（意図が不明な場合）
- **意味的コンフリクト**: 構文上は競合しないが、両側の変更が論理的に矛盾

判断に自信がある場合は自動で進め、解消内容を報告する。

## 注意事項
1. rebase中の中断: `git rebase --abort` で元の状態に戻せることを把握しておく
2. force push: rebase後はリモートと履歴が乖離するため、本スキル自身がステップ16で `git push --force-with-lease` を使ってプッシュする。`--force` は使わない（leaseによる上書き防止が無効になるため）
3. 共有ブランチでの注意: 他者と共有しているブランチでは事前確認が望ましい
4. leaseの限界: 引数なしの `--force-with-lease` はremote-tracking ref（`origin/[branch-name]`）をleaseに使うため、**リモートの更新をfetch済みで手元に取り込んでいない場合はpushが通ってしまう**（leaseがfetch後のrefと一致するため）。このリポジトリではレビュー系のコマンド（`ccx pr freshness`・`ccx worktree resolve`）がPRのhead branchをfetchし、remote-tracking refは全worktree・全セッションで共有されるため、この経路は現実に成立する。ステップ16のプッシュ前ゲートがこの穴を塞ぐ役割で、判定にAPI由来の `head_oid` を使うのはfetchの影響を受けないため
