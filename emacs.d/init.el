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

;;; elisp byte compile
(defun after-save-byte-compile ()
  (when (eq major-mode 'emacs-lisp-mode) (byte-compile-file buffer-file-name t)))
(add-hook 'after-save-hook 'after-save-byte-compile)

;;; bash*, .bash*.local -> sh-mode
(add-to-list 'auto-mode-alist '("bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bash_profile\\.local\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.bashrc\\.local\\'" . sh-mode))

;;; conf-mode is 8 tab
(add-hook 'conf-mode-hook (lambda () (setq indent-tabs-mode t) (setq tab-width 8)))

;;;; install package settings

;;; editorconfig
(editorconfig-mode 1)

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

;; indent
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook (lambda () (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

;;; php-mode
(add-hook 'php-mode-hook (lambda ()
                           (setq c-basic-offset 4)
                           (c-set-offset 'arglist-cont 0)
                           (c-set-offset 'arglist-cont-nonempty 0)
                           (c-set-offset 'case-label '+)
                           (c-set-offset 'statement-cont '+)))

;;; sql-indent
(eval-after-load "sql" '(load-library "sql-indent"))

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; set engine
(setq web-mode-engines-alist '(("smarty" . "\\.tpl\\'")))

;; indent
(add-hook 'web-mode-hook
          (lambda () (setq web-mode-markup-indent-offset 2
                           web-mode-css-indent-offset 2)))

;;; yaml-mode
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;; other settings

;;; gocode
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;;; golint
(add-to-list 'load-path (concat (getenv "GOPATH") "/src/github.com/golang/lint/misc/emacs"))
(require 'golint)

;;; load download elisp
(add-to-list 'load-path "~/.dotfiles/dl/emacs")
(add-hook 'js2-jsx-mode-hook (lambda () (load "sgml-mode-patch")))
(add-hook 'php-mode-hook
          (lambda ()
            (require 'php-cs-fixer)
            (add-hook 'before-save-hook 'php-cs-fixer-before-save)
            (load "fixers")
            (setq php-cs-fixer-level-option "none")))
