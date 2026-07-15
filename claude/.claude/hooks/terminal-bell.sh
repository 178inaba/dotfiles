#!/bin/bash

# 任意のフックイベントから端末ベルを鳴らす（terminalSequence 出力）
#
# フックは controlling terminal を持たない独自セッションで実行されるため
# （Claude Code v2.1.139+）、/dev/tty へ \a を直接書けず、stdout も端末には
# 届かない。hook JSON の terminalSequence フィールド（v2.1.141+）で
# Claude Code に代理出力させる。tmux は BEL を受けて window の bell flag を
# 立てる（タブ点灯）。Ghostty はデフォルトでベル音を鳴らさないため、
# 音は Notification フック側の afplay が担う。

jq -nc '{terminalSequence: "\u0007"}'
