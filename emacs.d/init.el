;;;; setup
(add-to-list 'load-path "~/.emacs.d/setup")
(load "setup")

;;;; base settings

;;; not backup
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;;; bashrc -> sh-mode
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))

;;;; install package settings

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;;;; other settings

;;; golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
