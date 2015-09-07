;;;; setup
(add-to-list 'load-path "~/.emacs.d/setup")
(load "setup")

;;;; base settings

;;; tab is 4
(setq-default tab-width 4)

;;; not backup
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;;; bashrc, .bashrc.local -> sh-mode
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\.local\\'" . sh-mode))

;;;; install package settings

;;; gitconfig-mode
(add-to-list 'auto-mode-alist '("gitconfig\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("gitconfig\\.local\\'" . gitconfig-mode))

;;; go-eldoc
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;; go-mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)

;;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;;;; other settings

;;; gocode
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;; golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
