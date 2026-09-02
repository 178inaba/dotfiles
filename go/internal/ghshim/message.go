package ghshim

import "fmt"

// The guidance the guard writes when it refuses. It stays Japanese, as it was.
//
// The text is not transcribed from the shell source: the shim wrote it in
// unquoted heredocs, so what reached standard error had already had \$( and the
// escaped backticks resolved. The bytes below are the ones it produced, and the
// tests hold them against captures of the real output.

// repoFlagStep and repoFlagHint open and close the first rule's advice wherever
// -R is a way through, which is four of the five.
const (
	repoFlagStep = "  1. 対象リポジトリを -R owner/repo で明示して再実行する"
	repoFlagHint = `  ※ 現在のディレクトリの remote が正しいと確信できる場合も、
       gh repo view --json nameWithOwner -q .nameWithOwner
     で取得した値を -R に渡して明示する`
)

// notExplicitMessage is the first rule: the repository has to be named.
func notExplicitMessage(c command, argv []string, dir, remote string) string {
	return fmt.Sprintf(`gh の書き込み系サブコマンドは、対象リポジトリをコマンド上で明示することを必須としています。

実行しようとしたコマンド:
  %s

明示が確認できなかった箇所: gh %s %s
現在の作業ディレクトリ: %s
現在の origin remote: %s

このまま実行すると、上記の origin (もしくは gh が解決する remote) が対象になります。
別リポジトリの調査中などに cwd を取り違えると、意図しないリポジトリに書き込みが行われます。

対処:
%s
`, attemptedCommand(argv), c.noun, c.verb, dir, remote, explicitnessRecovery(c))
}

// explicitnessRecovery is the way through, taken from the same classification
// the test uses: offering only -R would read as unsatisfiable under gh repo,
// which has none, and offering a URL to a verb with no selector would be a way
// through that is not one.
func explicitnessRecovery(c command) string {
	switch classify(c) {
	case byFlagNotPositional:
		return fmt.Sprintf(`%s
       例: gh repo rename new-name -R owner/repo
     gh repo rename の位置引数は新しいリポジトリ名で、リポジトリの明示にはなりません。
%s`, repoFlagStep, repoFlagHint)

	case byPositional:
		return fmt.Sprintf(`  1. 対象リポジトリを位置引数で明示して再実行する
       例: gh repo %s owner/repo ...
     形式は OWNER/REPO・HOST/OWNER/REPO・リポジトリ URL のいずれかです。
     bare な REPO は gh が認証ユーザーで補完するため明示になりません。
  2. gh repo %s に -R/--repo はなく、環境変数によるリポジトリ指定も効きません。
     位置引数はフラグより前でも後ろでも構いません。`, c.verb, c.verb)

	case byFlagNoSelector:
		return fmt.Sprintf(`%s
       例: gh %s create -R owner/repo ...
     create は selector を取らないため、URL でリポジトリを示す形はありません。
%s`, repoFlagStep, c.noun, repoFlagHint)

	case byFlagOrURL:
		example := "https://github.com/owner/repo/issues/123"
		if c.noun == "pr" {
			example = "https://github.com/owner/repo/pull/123"
		}
		return fmt.Sprintf(`%s
       例: gh %s %s -R owner/repo ...
  2. 対象を完全な URL で指定する（URL にリポジトリが含まれます）
       例: gh %s %s %s ...
     番号のみ・ブランチ名は cwd の remote で解決されるため明示になりません。
%s`, repoFlagStep, c.noun, c.verb, c.noun, c.verb, example, repoFlagHint)

	default:
		return fmt.Sprintf(`%s
       例: gh %s %s -R owner/repo ...
%s`, repoFlagStep, c.noun, c.verb, repoFlagHint)
	}
}

// multilineBodyMessage is the second rule: a body of more than one line has to
// go through a file.
func multilineBodyMessage(c command, bf bodyFlags, argv []string) string {
	return fmt.Sprintf(`gh の書き込み系サブコマンドで複数行の本文を --%s/-%s で渡すことは禁止しています。

実行しようとしたコマンド:
  %s

理由:
  --%s "$(...)" と HEREDOC を組み合わせると引用符レイヤが重なり、
  誤ったエスケープ（バッククォート前の不要なバックスラッシュ等）が
  そのまま本文に残る事故が起きます。

対処:
  1. 本文を一時ファイル（scratchpad 等）に Write で書き出す
%s
`, bf.inlineLong, bf.inlineShort, attemptedCommand(argv), bf.inlineLong, bodyRecovery(c, bf))
}

// bodyRecovery is the alternative to an inline body, taken from the table so
// that every verb is offered one it actually has.
func bodyRecovery(c command, bf bodyFlags) string {
	switch bf.recovery {
	case recoverByComment:
		return fmt.Sprintf(`  2. 本文は gh %s comment --body-file で別途投稿し、%s は -%s なしで実行する
       例: gh %s comment -R owner/repo 123 --body-file /path/to/body.md
           gh %s %s -R owner/repo 123`,
			c.noun, c.verb, bf.inlineShort, c.noun, c.noun, c.verb)
	default:
		return fmt.Sprintf(`  2. --%s の代わりに --%s <path> を指定して再実行する
       例: gh %s %s -R owner/repo ... --%s /path/to/body.md`,
			bf.inlineLong, bf.fileLong, c.noun, c.verb, bf.fileLong)
	}
}

// unreadableBodyMessage is the refusal that comes before the two rules that
// read the body. The reason is spelled out because the fix differs: the file
// may not be written yet, the path may be wrong, or it may not be readable.
func unreadableBodyMessage(bf bodyFlags, argv []string, path, reason string) string {
	return fmt.Sprintf(`gh の書き込み系サブコマンドの本文ファイルを読めなかったため、gh を実行しませんでした。

実行しようとしたコマンド:
  %s

読めなかった本文ファイル: --%s %s
理由: %s

理由の詳細:
  本文を読めないと、素の #N の項番・引用された closing keyword を走査できません。
  gh 自身も本文ファイルを読めなければ API に触れる前にエラー終了するため、ここで
  ブロックしても、成功したはずのコマンドが失われることはありません。

対処:
  1. 本文をまだ書いていない場合: 先にファイルへ Write してから再実行する
  2. パスが違う場合: パスを確認して再実行する（相対パスは現在の作業ディレクトリ基準）
  3. 標準入力から本文を渡す場合: --%s - を使う
     （stdin は shim が読むと gh の分を消費するため、走査を諦めて素通しします）
`, attemptedCommand(argv), bf.fileLong, path, reason, bf.fileLong)
}

// bareHashRefsMessage is the third rule: numbering an argument list with #N.
func bareHashRefsMessage(distinct int, source string) string {
	return fmt.Sprintf(`gh の書き込み系サブコマンドの本文に、項番とみられる素の #N を検出しました
（#1〜#9 のうち %d 種類）。

検出元: %s

理由:
  素の #数字 は GitHub で Issue/PR への自動リンクになるため、項目の
  番号付け（指摘 #1, #2, ...）に使うと、無関係な Issue/PR に参照通知
  （mentioned 表示）が飛ぶ事故が起きます。通知は後から取り消せません。

対処:
  1. 項番が目的の場合: 順序リスト（1. 2. ...）等、# を使わない形式に書き換える
  2. 実際に Issue/PR を参照する意図の場合: OWNER/REPO#N 形式で明示する
       例: 178inaba/dotfiles#3
     （リンクは維持され、このガードにも掛かりません）
`, distinct, source)
}

// quotedClosingKeywordMessage is the fourth rule. It is written as an
// interpreted string because it quotes a backtick, which a raw one cannot hold.
func quotedClosingKeywordMessage(source string) string {
	return fmt.Sprintf("PR 本文中に、バッククォート（コードスパン/コードブロック）で囲まれた\n"+
		"GitHub closing keyword（Closes/Fixes/Resolves #N）を検出しました。\n"+
		"\n"+
		"検出元: %s\n"+
		"\n"+
		"理由:\n"+
		"  バッククォートで囲むと GitHub が closing keyword として解釈せず、\n"+
		"  PR をマージしても対象 Issue が自動 close されません（open のまま残る事故）。\n"+
		"\n"+
		"対処:\n"+
		"  1. Issue を自動 close する意図の場合: バッククォートを外して生のまま書く\n"+
		"       例: Closes #656\n"+
		"  2. closing keyword を引用・文書化する意図の場合: 実番号を避けて\n"+
		"     プレースホルダに置き換える（例: `Closes #N`。番号がなければ検出されません）\n",
		source)
}
