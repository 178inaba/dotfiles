---
name: git-push
description: コミット済みの変更をプッシュ
argument-hint: [branch-name]
disable-model-invocation: true
---

# /git-push

コミット済みの変更をプッシュ

## 使用方法
```
/git-push [branch-name]
```

## 実行内容
1. `git status` で未プッシュのコミットを確認
2. ブランチ名が指定された場合: `git push -u origin [branch-name]`を実行
   指定されない場合: `git push`で現在のブランチをプッシュ
3. プッシュ結果を確認（既存PRがある場合は自動的に更新される）
