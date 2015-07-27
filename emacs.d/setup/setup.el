;;;; load package.el(version 23 or less)
(when (file-exists-p "~/.emacs.d/dl/package.el")
  (add-to-list 'load-path "~/.emacs.d/dl")
  (load "package.el"))

;;;; install packages

;;; package list
(setq my-init-pkgs
      '(
	go-mode
	markdown-mode
	js2-mode
	json-mode
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
