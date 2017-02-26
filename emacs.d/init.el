;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;;;; Setup.
(add-to-list 'load-path "~/.emacs.d/setup")
(load "setup")

;;;; Base settings.

;;; Hide menu bar.
(menu-bar-mode 0)

;;; Display column number.
(column-number-mode t)

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

;;; coffee-mode
(add-hook 'coffee-mode-hook (lambda () (setq coffee-tab-width 2)))

;;; editorconfig
(editorconfig-mode 1)

;;; flycheck
(add-hook 'go-mode-hook #'global-flycheck-mode)

;;; git-gutter+
(global-git-gutter+-mode)

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
                           (c-set-offset 'arglist-cont-nonempty '+)
                           (c-set-offset 'case-label '+)
                           (c-set-offset 'statement-cont '+)))

;;; sql-indent
(eval-after-load "sql" '(load-library "sql-indent"))
(add-hook 'sql-mode-hook (lambda () (setq sql-indent-offset 2)))

;;; typescript-mode
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

;;; web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))

;; set engine
(setq web-mode-engines-alist '(("go" . "\\.tmpl\\'")))
(setq web-mode-engines-alist '(("django" . "\\.tpl\\'")))

;; indent
(add-hook 'web-mode-hook
          (lambda () (setq web-mode-markup-indent-offset 2
                           web-mode-css-indent-offset 2)))

;;; yaml-mode
(add-hook 'yaml-mode-hook
          (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-gutter+ csharp-mode lua-mode yaml-mode web-mode typescript-mode toml-mode sql-indent php-mode php-completion perl6-mode nginx-mode markdown-mode json-mode js2-mode go-mode go-eldoc gitconfig-mode flycheck editorconfig dockerfile-mode coffee-mode auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
