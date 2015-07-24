;;;; base settings

;;; not backup
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;;; bashrc -> sh-mode
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))

;;;; install packages

;;; package list
(setq my-init-pkgs
      '(
	markdown-mode
	))

;;; init
(package-initialize)

;;; add repo
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))

;;; package installed check
(setq my-init-pkg-install nil)
(catch 'end
  (dolist (pkg my-init-pkgs)
    (unless (package-installed-p pkg)
      (setq my-init-pkg-install t)
      (throw 'end t))))

;;; install
(when my-init-pkg-install
  (package-refresh-contents)
  (dolist (pkg my-init-pkgs) (package-install pkg)))

;;;; install package settings

;;; markdown-mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
