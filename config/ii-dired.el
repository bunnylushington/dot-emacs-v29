(use-package dired
  :config
  (setq insert-directory-program
        (if (executable-find "gls") "gls" "ls"))

  (set-face-attribute 'dired-directory nil
                      :foreground (nord-color "aurora-2"))
  (setq dired-use-ls-dired t
        dired-movement-style 'cycle
        dired-listing-switches
        "-lhAG --group-directories-first"
        dired-kill-when-opening-new-dired-buffer t
        dired-vc-rename-file t))

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<C-tab>" . dired-subtree-cycle)))

(use-package dired-collapse
  :straight t
  :after dired
  :config
  (add-hook 'dired-mode-hook 'dired-collapse-mode))
