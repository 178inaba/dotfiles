# git ステージング分割が困難な場合のフォールバック

`git add -p` の `s` (split) が「Sorry, cannot split this hunk」で失敗した場合、または変更行間に context line が無く自動分割できない場合の代替手段。

優先順位（上から順に試す）。

## 1. `e` (edit mode): 対話的にパッチを編集

`git add -p` 中に `e` を選択するとエディタで hunk を直接編集できる。
ステージングしたい行を残し、不要な行を編集（追加行 `+` は削除、削除行 `-` は ` ` に変更）。

## 2. 手書きパッチ + `git apply --cached`

```bash
git diff path > /tmp/p.patch
# /tmp/p.patch から不要な hunk を削除（エディタ編集）
git apply --cached --check /tmp/p.patch  # 事前検証
git apply --cached /tmp/p.patch
```

CLI で完結させたい場合や、複数 hunk を機械的に取捨選択したい場合に有効。

## 3. ファイル直接書き換え（中間状態がビルド可能な場合のみ）

一部の変更を一旦リバート → 残った変更をステージング → コミット → リバートを戻す。
中間状態でコンパイル・テストが通る前提でしか使えない。

## 4. `GIT_EDITOR='sed -i ...' git add -p`

非対話で sed 編集を埋め込む。CI / スクリプト向けで、通常の対話作業では出番が少ない。

## 中止判断

上記いずれも困難、または中間状態がビルド不能な場合は、**分割を諦めて 1 コミットにまとめる**。
git-commit スキルの「自己完結原則」（各コミット単体でビルド・テストが通る）に従う。

## 参考

- [git-add Documentation](https://git-scm.com/docs/git-add)
- [git-apply Documentation](https://git-scm.com/docs/git-apply)
- [Git Interactive Staging - Pro Git Book](https://git-scm.com/book/en/v2/Git-Tools-Interactive-Staging)
