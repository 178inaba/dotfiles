;;; not backup
(setq make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

;;; bashrc -> sh-mode
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
