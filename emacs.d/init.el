;;;; setup
(add-to-list 'load-path "~/.emacs.d/setup")
(load "setup")

;;;; base settings

;;; indent is 4, not tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;; not backup
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;;; delete space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; bash*, .bash*.local -> sh-mode
(add-to-list 'auto-mode-alist '("bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile\\.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\.local\\'" . sh-mode))

;;; conf-mode is 8 tab
(add-hook 'conf-mode-hook (lambda () (setq indent-tabs-mode t) (setq tab-width 8)))

;;;; install package settings

;;; flycheck
(add-hook 'go-mode-hook #'global-flycheck-mode)

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
(add-to-list 'auto-mode-alist '("/jsx/.+\\.js\\'" . js2-jsx-mode))
(setq js2-basic-offset 2)

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook (lambda () (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

;;; sql-indent
(eval-after-load "sql" '(load-library "sql-indent"))

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))

;; indent
(add-hook 'web-mode-hook
          (lambda () (setq web-mode-markup-indent-offset 2
                           web-mode-css-indent-offset 2)))

;; set engine
(setq web-mode-engines-alist '(("smarty" . "\\.tpl\\'")))

;;; yaml-mode
(add-hook 'yaml-mode-hook (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;; other settings

;;; gocode
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;; golint
(add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)
