;;;; install packages

;;; package list
(setq my-init-pkgs
	  '(
		auto-complete
		dockerfile-mode
		flycheck
		gitconfig-mode
		go-eldoc
		go-mode
		js2-mode
		json-mode
		markdown-mode
		nginx-mode
		toml-mode
		web-mode
		yaml-mode
		))

;;; load package.el
(require 'package)

;;; add repo
(add-to-list 'package-archives
			 '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
			 '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

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
