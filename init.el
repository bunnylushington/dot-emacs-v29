;; Configured for emacs-plus:
;;  https://github.com/d12frosted/homebrew-emacs-plus

;; Some basic setup.
(setq byte-compile-warnings '(not obsolete)
      warning-suppress-log-types '((comp) (bytecomp))
      warning-minimum-level :error
      frame-title-format "%b"
      font-lock-maximum-decoration t
      mac-use-title-bar t
      default-directory "~/"
      widget-image-enable nil
      tab-width 2
;      tab-always-indent 'complete
      calendar-latitude 29.9510
      calendar-longitude -90.0715)

(midnight-mode)
(pixel-scroll-precision-mode)
(add-to-list 'completion-ignored-extensions ".#")
(global-goto-address-mode)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
; (desktop-save-mode 1)

;; Functions to help load path construction.
(defun ii/emacs-dir-file (file)
  "Concatenate FILE to user-emacs-directory."
  (expand-file-name file user-emacs-directory))

(defun ii/home-dir-file (file)
  "Concatenate FILE to $HOME."
  (expand-file-name file (getenv "HOME")))

;; Paths
(setq ii/exec-path
      `("/usr/local/bin"
        "/opt/homebrew/bin"
        "/opt/homebrew/opt/mysql-client/bin"
        ,(ii/home-dir-file "go/bin")))
(mapc (lambda (path) (add-to-list 'exec-path path)) ii/exec-path)

;; Archives
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("MELPA" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA" . 5)
        ("nongnu" . 10)
        ("gnu" . 0)))

;; Expansion
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))

;; MacOS function keys.
(if (eq system-type 'darwin)
    (setq mac-option-modifier '(:function alt :mouse alt)
          mac-right-command-modifier 'super
          mac-right-option-modifier 'hyper
          ns-alternate-modifier 'super
          ns-command-modifier 'meta))

;; Quit emacs.
(global-set-key (kbd "C-x C-z") 'save-buffers-kill-terminal)

;;;; Behaviors
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; Allow narrow-to-region
(put 'narrow-to-region 'disabled nil)

;; Always backspace, never erase
(global-unset-key (kbd "<S-delete>"))
(normal-erase-is-backspace-mode 0)

;; dired
(use-package dired
  :config
  (setq dired-use-ls-dired nil
        dired-vc-rename-file t))

;; all the icons
(use-package all-the-icons
  :ensure t
  :config
  ;; Use 'prepend for the NS and Mac ports or Emacs will crash.
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'prepend))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; ibuffer
(use-package ibuffer
  :ensure t
  :config
  (setq ibuffer-default-sorting-mode 'major-mode)
  :bind ([remap list-buffers] . ibuffer))

;; url
(use-package url
  :ensure t
  :config
  (defun ii/web-search ()
    "Search DuckDuckGo from Emacs."
    (interactive)
    (let* ((term (read-string "Search term: "))
           (url (format "\"https://ddg.gg?q=%s\"" (url-hexify-string term)))
           (cmd (concat "open " url)))
      (start-process-shell-command "" nil cmd)))

  (defun ii/tinyurl ()
    "Create a tiny URL."
    (interactive)
    (let* ((long-url (thing-at-point 'url))
           (tinyurl
            (save-excursion
              (with-temp-buffer
                (mm-url-insert
                 (concat "http://tinyurl.com/api-create.php?url=" long-url))
                (kill-ring-save (point-min) (point-max))
                (buffer-string)))))
      (message tinyurl)))

  :bind ("C-c C-s" . ii/web-search))

;; dim other windows
(use-package auto-dim-other-buffers
  :ensure t
  :custom
  (auto-dim-other-buffers-affected-faces
   '((default . auto-dim-other-buffers-face)
     (org-block . auto-dim-other-buffers-face)
     (org-hide . auto-dim-other-buffers-face)
     (fringe . auto-dim-other-buffers-face)))
  :config
  (auto-dim-other-buffers-mode))


;; parens
(use-package paren
  :ensure t
  :config
  (show-paren-mode t)
  (setq show-paren-style 'expression
        show-paren-context-when-offscreen 'overlay
        show-paren-priority 9999))

;; fill column indicator
(global-set-key (kbd "C-c w") 'display-fill-column-indicator-mode)
(setq display-fill-column-indicator-character 124
      display-fill-column-indicator-column 80)

;; Junk File Utilities
(global-set-key (kbd "C-x j") 'open-junk-file)
(global-set-key (kbd "C-x C-j") 'ii/open-current-junk-directory)
(global-set-key (kbd "C-x M-j") 'ii/rgrep-junk-directory)

(defun ii/open-current-junk-directory ()
  "Dired the most relevant junk directory."
  (interactive)
  (let* ((full-filename (format-time-string open-junk-file-format))
         (directory (file-name-directory full-filename)))
    (dired directory)))

(defun ii/rgrep-junk-directory ()
  "Run rgrep over the entire junk directory."
  (interactive)
  (let ((pattern (read-string "Pattern: "))
        (directory "~/junk"))
    (rgrep pattern "*" directory)))

;; Grep Find Setup
(use-package grep
  :bind ("C-x C-g" . grep-find)
  :config
  (grep-apply-setting
   'grep-find-command
   '("rg -n -H --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27)))


(use-package deadgrep
  :ensure t
  :bind ("H-g" . deadgrep))

;; Convenience Key Bindings
(global-set-key (kbd "M-C-<down>") 'scroll-other-window)
(global-set-key (kbd "M-C-<up>") 'scroll-other-window-down)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-q") 'shortdoc-display-group)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-u") 'uuidgen)
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)
(global-set-key (kbd "s-d") 'osx-dictionary-search-input)

;; Window resizing
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)

;; Other window conveniences
(defun ii/toggle-window-dedication ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window)
                          (not (window-dedicated-p (selected-window)))))


;; Backup
(setq vc-make-backup-files t)
(defun ii/save-buffer-force-backup (arg)
  "Force a backupp on each save-buffer."
  (interactive "P")
  (if (consp arg) (save-buffer) (save-buffer 16)))
(global-set-key [remap save-buffer] 'ii/save-buffer-force-backup)
(global-set-key (kbd "C-x s") 'ii/save-buffer-force-backup)

;;;; WTF?
(defface bookmark-menu-heading '((t  :inherit default-face))
  "Apparently a missing face (so far) in v29")

(defface dired-directory-face '((t  :inherit default-face))
  "Apparently a missing face (so far) in v29")

;;;; Fix emacs-mac info
;; For some reason emacs-mac doesn't generate a dir file in
;; /opt/homebrew/Cellar/emacs-mac/emacs-28.1-mac-9.0/share/info/emacs
;;
;; Fix this with
;;   for F in `echo *.info.gz`; do
;;       install-info $F dir
;;   done
;;
;; It also appears this nonsense is required to actually READ that
;; list (even though we don't change the info path at all?).
(use-package info
  :ensure t
  :config
  (info-initialize))

;; Vertico
(use-package vertico
  :ensure t
  :bind (:map vertico-map
              (("<backspace>"   . vertico-directory-delete-char)
               ("C-w"           . vertico-directory-delete-word)
               ("C-<backspace>" . vertico-directory-delete-word)
               ("<return>"      . vertico-directory-enter)))
  :hook
  ((minibuffer-setup           . vertico-repeat-save)
   (rfn-eshadow-update-overlay . vertico-directory-tidy))
  :custom
  (vertico-count 14)
  (vertico-resize nil)
  (vertico-cycle nil)
  :init
  (require 'vertico-buffer)
  (fido-mode -1)
  (vertico-mode))


;; Marginalia
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu
;;
;; see https://github.com/minad/corfu

(use-package corfu
  :ensure t
  :custom
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-on-exact-match t)
  (corfu-echo-documentation nil)
  :config
  (global-corfu-mode))

(use-package corfu-popupinfo
  :custom
  (corfu-popupinfo-min-width 40)
  (corfu-popupinfo-max-width 90)
  (corfu-popupinfo-min-height 2)
  (corfu-popupinfo-delay t)
  (corfu-popupinfo-hide t)
  :bind
  (("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down))
  :config
  (corfu-popupinfo-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Consult
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)
         ("M-s l" . consult-line)
         ("M-s d" . consult-find)
         ("C-x r b" . consult-bookmark)
         ("C-c m" . consult-mode-command)
         ("M-y" . consult-yank-pop)))

;; (use-package consult-eglot
;;   :ensure t
;;   :bind ("H-s" . consult-eglot-symbols))

;; Orderless
(use-package orderless
  :ensure t)

;; JSON
(use-package json-mode
  :after flymake-json
  :ensure t)

;; Magit
(use-package magit
  :ensure t
  :demand t
  :bind (("s-g" . 'magit-status))
  :hook (before-save . magit-wip-commit-initial-backup)
  :config
  (setq magit-commit-show-diff nil)
  (magit-wip-mode 1))

(use-package ghub
  :ensure t)

;; Forge
;; (use-package forge
;;   :ensure t
;;   :after magit)


;; Show (and act on) changed hunks.
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode +1))

;; YAML
(use-package yaml-mode
  :ensure t)

;; vterm
(use-package vterm
  :ensure t
  :demand t
  :bind
  ((:map vterm-mode-map ("s-c" . vterm-copy-mode))
   (:map vterm-copy-mode-map ("s-c" . vterm-copy-mode)))
  :config
  (add-to-list 'vterm-eval-cmds
               '("update-pwd" (lambda (path) (setq default-directory path))))
  (setq vterm-toggle-fullscreen-p nil
        vterm-toggle-hide-method nil
        disabled-command-hook nil
        vterm-clear-scrollback-when-clearing t
        vterm-environment '("'(\"emacs-vterm=true\")'")
        vterm-max-scrollback 10000))

(use-package multi-vterm
  :ensure t
  :bind (("s-n" . multi-vterm)
         ("s-," . multi-vterm-prev)
         ("s-." . multi-vterm-next)))



;; Quick Buffer Switch
;;
;; Don't forget how useful C-x C-c C-/ is.
(use-package quick-buffer-switch
  :ensure t
  :config
  (qbs-init)
  (qbs-add-predicates
   (make-qbs:predicate
    :name 'go
    :shortcut "C-g"
    :test '(when (eq major-mode 'go-mode) qbs:buffer-name))
   (make-qbs:predicate
    :name 'web
    :shortcut "C-w"
    :test '(when (eq major-mode 'web-mode) qbs:buffer-name))
   (make-qbs:predicate
    :name 'sql
    :shortcut "C-s"
    :test '(when (eq major-mode 'sql-mode) qbs:buffer-name))
   (make-qbs:predicate
    :name 'eshell
    :shortcut "C-c"
    :test '(when (eq major-mode 'eshell-mode) qbs:buffer-name))
   (make-qbs:predicate
    :name 'vterm
    :shortcut "C-v"
    :test '(when (eq major-mode 'vterm-mode) qbs:buffer-name))))

;; Whitespace
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(trailing tabs))
  (global-whitespace-mode t)
  :hook (before-save . delete-trailing-whitespace))

;; Detached
(setq nano-modeline-mode-formats nil)
(use-package detached
  :ensure t
  :init
  (detached-init)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

;; macos lacks dbus, so use the alt notification system (alert)
(if (eq system-type 'darwin)
    (setq detached-notification-function #'detached-extra-alert-notification))

;; Perl
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil)
(add-to-list 'auto-mode-alist '("\\.t$"  . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.p[lm]" . cperl-mode))

;; Web
(use-package web-mode
  :ensure t
  :after hl-todo
  :mode (("\\.tpl\\.php\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.dtl\\'" . web-mode)
         ("\\.heex\\'" . web-mode)
         ("\\.eex\\'" . web-mode))
  :config
  (setq web-mode-engines-alist '(("django" . "\\.dtl\\'")))
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-quoting nil
        web-mode-markup-indent-offset 2
        web-mode-sql-indent-offset 2)
  (add-to-list 'hl-todo-exclude-modes 'web-mode))

;; HL
(use-package hl-todo
  :ensure t
  :bind
  (:map hl-todo-mode-map
        (("C-c o" . hl-todo-occur)
         ("C-c i" . hl-todo-insert)))
  :config
  (global-hl-todo-mode 1)

  (defun ii/hl-todo-insert (keyword)
    "Advice around hl-todo-insert"
    (interactive
     (list (completing-read
            "Insert keyword: "
            (cl-mapcan (pcase-lambda (`(,keyword . ,face))
                         (and (equal (regexp-quote keyword) keyword)
                              (list (propertize keyword 'face
                                                (hl-todo--combine-face face)))))
                       hl-todo-keyword-faces))))
    (move-end-of-line nil)
    (newline)
    (indent-for-tab-command)
    (insert (concat keyword ": "))
    (back-to-indentation)
    (set-mark-command nil)
    (move-end-of-line nil)
    (comment-dwim nil))

  (advice-add 'hl-todo-insert :override #'ii/hl-todo-insert))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1))

;; Docker
(use-package docker
  :ensure t
  :bind (("C-c C-d" . docker)))

;; Flymake
(use-package flymake
  :ensure t)

;; ElDoc
(use-package eldoc
  :ensure t
  :custom
  (eldoc-echo-area-prefer-doc-buffer t))

;; Project
(use-package project)

;; ;; Eglot
;; (use-package eglot)

;; LSP-Mode
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "s-l")
  (defun ii/lsp-mode-setup-completion()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook ((go-mode . lsp)
         (go-ts-mode . lsp)
         (elixir-mode . lsp)
         (python-mode . lsp)
         (lsp-completion-mode . ii/lsp-mode-setup-completion))
  :custom
  (lsp-completion-provider :none)
  (lsp-modeline-diagnostics-enable nil)
  :config
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-peek-enable t))

(use-package flycheck
  :ensure t)


;; Go
;;
;; Requires
;;   go install github.com/lukehoban/go-outline@latest
;;   go install golang.org/x/tools/gopls@latest
;;   go install github.com/go-delve/delve/cmd/dlv@latest
;;   ln -s ~/go/bin/gopls ~/.local/bin
(use-package go-ts-mode
  :ensure t
  :after (treesit-langs lsp-mode)
  :config
  (defun ii/lsp-go-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :custom
  (go-ts-mode-indent-offset 2)
  :hook ((go-mode . tree-sitter-hl-mode)
         (go-mode . ii/lsp-go-save-hooks)))

;; Go REPL
;;
;; go install github.com/x-motemen/gore/cmd/gore@latest
(use-package gorepl-mode
  :ensure t)

;; Python
;;
;; Requires
;;  pip3 install jedi autopep8 flake8 ipython importmagic yapf
(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt")
  :hook (elpy-mode . (lambda ()
                       (add-hook 'before-save hook
                                 'elpy-format-code nil t))))

;; Elm
(use-package elm-mode
  :ensure t
  :config
  (setq elm-indent-after-keywords
        '(("of" 2) ("in" 2 0) ("{" 2) "if" "then" "else" "let")
        elm-indent-offset 2
        elm-sort-imports-on-save t))

;; Erlang
(use-package erlang
  :ensure t
  :config
  (setq erlang-check-module-name t
        erlang-indent-level 2)
  (defun ii/set-erlang-indent-level (spaces)
    "Change the Erlang indentation level."
    (interactive "nIndention Level: ")
    (set-variable 'erlang-indent-level spaces t)))

;; Elixir
(use-package elixir-mode
  :ensure t)
  ;; :config
  ;; (let ((elixir-ls (ii/emacs-dir-file "elixir-ls/release/language_server.sh")))
  ;;   (add-to-list 'eglot-server-programs `(elixir-ts-mode ,elixir-ls))
  ;;   (add-to-list 'eglot-server-programs `(elixir-mode ,elixir-ls)))
  ;; :hook (elixir-mode . eglot-ensure))

;; Hydra
(use-package hydra
  :ensure t
  :demand t
  :bind (("s-i" . ip4g/hydra/body)))

;; IP4G
;;
;; NB: it's important hydra set `:demand t` to provide
;; the necessary macros ip4g requires.
(let ((ip4g-dir
       (expand-file-name "~/projects/converge/morpho-utils/emacs")))
  (if (file-directory-p ip4g-dir)
      (progn
        (add-to-list 'load-path ip4g-dir)
        (require 'ip4g))))

;; Tramp
(use-package tramp
  :ensure t
  :config
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>❯] *\\(\\[[[:digit:];]*[[:alpha:]] *\\)*"))

;; Smart Comment
(use-package smart-comment
  :ensure t
  :config
  :bind ([remap comment-dwim] . smart-comment))

;; Emacs Server
(use-package server
  :ensure t
  :config
  (unless (server-running-p) (server-start)))

;; Maybe some better window switching management
;;
;; Note that a submenu can be realized with ?
(use-package ace-window
  :ensure t
  :custom
  (aw-keys '(49 50 51 52 53 54 55 56 57))
  :bind (("M-o" . ace-window)
         ("C-<return>" . ace-window)))

(defun ii/split-below (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'below nil))

(defun ii/split-right (arg)
  "Split window below from the parent or from root with ARG."
  (interactive "P")
  (split-window (if arg (frame-root-window)
                  (window-parent (selected-window)))
                nil 'right nil))

;; Markdown preview
(use-package impatient-mode
  :ensure t
  :config
  (defun ii/markdown-html (buffer)
    (princ (with-current-buffer buffer
             (format "<!DOCTYPE html><html><title>Impatient Markdown</title>
<xmp theme=\"united\" style=\"display:none;\"> %s
</xmp><script src=\"http://ndossougbe.github.io/strapdown/dist/strapdown.js\">
</script></html>" (buffer-substring-no-properties (point-min) (point-max))))
           (current-buffer)))
  (add-to-list 'imp-default-user-filters `(markdown-mode . ii/markdown-html))
  (add-to-list 'imp-default-user-filters `(gfm-mode . ii/markdown-html)))

(use-package simple-httpd
  :ensure t
  :custom
  (httpd-port 9999)
  :config
  (httpd-start))

;; Markdown
(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . impatient-mode)
  :bind ("H-i" . ii/preview-md)
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . gfm-mode)))

(defun ii/preview-md ()
  "Preview an impatient-mode buffer in a browser."
  (interactive)
  (browse-url (format "http://localhost:9999/imp/live/%s" (buffer-name))))

;; Restclient
(use-package restclient
  :ensure t
  :mode "\\.rest\\'")

;; RSS
(use-package elfeed
  :ensure t
  :disabled
  :config
  (setq
   elfeed-log-level 'info
   elfeed-goodies/entry-pane-position 'top
   elfeeds-feeds
   '(
     ("https://www.reddit.com/r/qlab.rss" qlab)
     ("https://www.reddit.com/r/techtheater.rss" techtheater)
     ("https://www.reddit.com/r/throwers.rss" throwers)
     ("https://www.reddit.com/r/emacs.rss" emacs)))
  (setq-default elfeed-search-filter "@60-days-ago +unread"))

;; Alert
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'osx-notifier))

;; Treesitter
(use-package tree-sitter
  :ensure t
  :config
  (global-tree-sitter-mode)
  (set-face-attribute 'tree-sitter-hl-face:function nil
                      :inherit font-lock-function-name-face
                      :foreground "coral1"
                      :slant 'normal
                      :height 1.3
                      :width 'normal)

  (set-face-attribute 'tree-sitter-hl-face:method nil
                      :inherit 'tree-sitter-hl-face:function)

  (set-face-attribute 'tree-sitter-hl-face:string nil
                      :inherit 'font-lock-string-face
                      :foreground "RosyBrown2")

  (set-face-attribute 'tree-sitter-hl-face:type nil
                      :inherit 'font-lock-type-face
                      :foreground "MistyRose4")
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


;; There some weirdness here.  You'd *expect* that the shared objects
;; would be named correctly (and installed in a reasonable path) but I
;; don't think that's the case.  After we install the grammars, let's
;; fix up the load-path and make some symlinks.

;; Obviously this is just a mess.  It turns out that when the grammars
;; are updated things go wonky.  I've had some success deleting the
;; elpa/tree-sitter-langs... directory and restarting emacs (twice).
;; Fixing all this up remains a low-key todo.

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (tree-sitter-langs-install-grammars)

  (defun ii/tree-sitter-fixup-grammars ()
    "Prepare tree-sitter-langs for actual use."
    (interactive)
    (require 'f)
    (let ((grammar-dir (concat tree-sitter-langs-grammar-dir "bin")))
      (add-to-list 'treesit-extra-load-path grammar-dir)
      (mapc
       (lambda (f)
         (if (and (s-ends-with-p ".dylib" f)
                  (not (s-contains-p "libtree-sitter-" f)))
             (let* ((f-name (file-name-nondirectory f))
                    (link-name (concat "libtree-sitter-" f-name))
                    (link-path (f-join (file-name-directory f) link-name)))
               (f-delete link-path t)
               (f-copy f link-path))))
       (f-entries grammar-dir))))

  (ii/tree-sitter-fixup-grammars)
  (push '(yaml-ts-mode . yaml) tree-sitter-major-mode-language-alist)
  (push '(json-ts-mode . json) tree-sitter-major-mode-language-alist)
  (push '(erlang-mode  . erlang) tree-sitter-major-mode-language-alist)
  (push '(cperl-mode   . perl) tree-sitter-major-mode-language-alist)
  (push '(elixir-mode  . elixir) tree-sitter-major-mode-language-alist)
  (push '(go-ts-mode   . go) tree-sitter-major-mode-language-alist))

(use-package sql
  :ensure t
  :mode "\\.eqlite\\'"
  :config
  (setq sql-product 'postgres
        sql-mysql-options '("--protocol=tcp")))

(use-package outline-magic
  :ensure t)

(use-package outline
  :ensure t
  :after outline-magic
  :config
  (define-key outline-minor-mode-map (kbd "<C-tab>") 'outline-cycle))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package ace-kill
  :load-path "~/.emacs.d/ace-kill"
  :bind ("s-u" . ace-kill-hydra/body))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))



;; Bookmark+ Configuration
;;
;; Only loads if bookmark+ has been cloned to
;; user-emacs-directory/bookmark-plus.
(let ((bookmark-plus-dir (ii/emacs-dir-file "bookmark-plus")))
  (if (file-directory-p bookmark-plus-dir)
      (progn
        (add-to-list 'load-path bookmark-plus-dir)
        (require 'bookmark+)
        (setq bookmark-version-control t))))

;; Nano Theme Configuration.
;;
;; Only loads if nano-emacs has been cloned to
;; user-emacs-directory/nano-emacs.
(let ((nano-emacs-dir (ii/emacs-dir-file "nano-emacs")))
  (if (file-directory-p nano-emacs-dir)
      (progn
        (add-to-list 'load-path nano-emacs-dir)
        (setq nano-font-family-monospaced "Monaco"
              nano-font-size 12)
        (require 'nano-layout)
        (require 'nano-theme-dark)
        (nano-theme-set-dark)
        (require 'nano-faces)
        (nano-faces)
        (require 'nano-theme)
        (nano-theme)
        (require 'nano-defaults)
        (require 'nano-session)
        (require 'nano-modeline)
        (require 'nano-defaults))))

;; Layout
(setcdr (assq 'internal-border-width default-frame-alist) 12)
(setcdr (assq 'menu-bar-lines default-frame-alist) 0)

;; Session
(setq backup-directory-alist `((".*" . ,(ii/emacs-dir-file ".backups")))
      vc-make-backup-files t
      bookmark-default-file (ii/emacs-dir-file "bookmarks")
      backup-enable-predicate (lambda (name) t))

;; Face tweaks
(set-face-attribute 'nano-face-critical nil
                    :foreground nano-color-subtle
                    :background nano-color-critical)

;; Fringe
(fringe-mode)
(setq indicate-buffer-boundaries 'left
      indicate-empty-lines t
      x-underline-at-descent-line t
      global-linum-mode nil)
(setq-default left-fringe-width 15)
(window-divider-mode 0)


(defun ii/what-face (pos)
  "Show face at point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun ii/hex-region (start end)
  "urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-hexify-string))

(defun ii/unhex-region (start end)
  "de-urlencode the region between START and END in current buffer."
  (interactive "r")
  (func-region start end #'url-unhex-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slack
(use-package slack
  :ensure t
  :bind   ("s-s" . slack-mode-hydra/body)
  :config
  (load-file (ii/emacs-dir-file "ii-slack.el"))
  (slack-register-team
   :name ii/slack-team-name
   :default 't
   :token (auth-source-pick-first-password
           :host '("slack-emacs")
           :user "token"
           :type 'netrc
           :max 1)
   :cookie (auth-source-pick-first-password
            :host '("slack-emacs")
            :user "cookie"
            :type 'netrc
            :max 1)
   :mark-as-read-immediately nil
   :full-and-display-names 't
   :visible-threads 't
   :subscribed-channels ii/slack-subscribed-channels)
  (slack-start)
  (define-key lui-mode-map (kbd "<return>") 'newline)
  (define-key lui-mode-map (kbd "M-<return>") 'lui-send-input)
  (setq
   lui-fill-column 78
   lui-time-stamp-position 'right-margin
   lui-flyspell-p nil
   lui-fill-type nil
   slack-buffer-emojify 't
   slack-display-team-name nil
   slack-enable-wysiwyg 't
   slack-file-dir "~/Downloads"
   slack-mrkdwn-blockquote-sign ""
   slack-buffer-create-on-notify 't
   slack-prefer-current-team 't
   slack-render-image-p nil
   slack-thread-also-send-to-room nil)

  (defun ii/lui-setup ()
    (setq fringes-outside-margins t
          right-margin-width 8
          word-wrap t
          wrap-prefix ""))
  (add-hook 'lui-mode-hook 'ii/lui-setup)
  (setf (cdr (assoc 'continuation fringe-indicator-alist)) nil)

  (require 'slack-message-faces)
  (require 'slack-mrkdwn)
  (set-face-attribute 'slack-message-output-header nil
                      :underline nil
                      :height 1.1)

  (set-face-attribute 'lui-button-face nil
                      :underline nil
                      :foreground "DeepSkyBlue1")

  (set-face-attribute 'slack-preview-face nil
                      :inherit 'default
                      :height 1
                      :foreground "pink")

  (set-face-attribute 'slack-mrkdwn-code-block-face nil
                      :inherit 'default
                      :height 1
                      :foreground "pink")

  (set-face-attribute 'slack-message-output-text nil
                      :height 1.1
                      :family "Verdana")

  (defhydra slack-mode-hydra (:color pink
                                     :exit t
                                     :hint nil)
    "
  ^Buffers^                ^Editing
  ^^^^^^^^^^------------------------------
  _a_: show all threads    _e_: add reaction
  _u_: unread rooms        _c_: compose in buffer
  _i_: select IM           _t_: show/create thread
  _r_: select room         _y_: region to code block
  "
    ("a" slack-all-threads)
    ("u" slack-select-unread-rooms)
    ("i" slack-im-select)
    ("r" slack-select-rooms)
    ("e" slack-message-add-reaction)
    ("c" slack-message-write-another-buffer)
    ("t" slack-thread-show-or-create)
    ("y" ii/slack-copy-to-buffer)
    ("q" nil "quit" :color blue))


  (defun ii/slack-copy-to-buffer (buffer start end)
    "Copy region to selected BUFFER"
    (interactive "BCopy to buffer: \nr")
    (let ((oldbuf (current-buffer)))
      (with-current-buffer (get-buffer-create buffer)
        (barf-if-buffer-read-only)
        (save-excursion
          (insert "\n```\n")
          (insert-buffer-substring oldbuf start end)
          (insert "\n```\n")))))

  ;; NB: re-builder to the rescue!
  (defconst ii/slack-mrkdwn-regex-link "\\[\\([^]]+\\)\\](\\([^)]+\\))")

  ;; convert [link-label](link-url) to a "link-label" lui-button
  (defun ii/slack-mrkdwn-add-link ()
    (goto-char (point-min))
    (while (re-search-forward ii/slack-mrkdwn-regex-link (point-max) t)
      (let* ((label-beg (match-beginning 1))
             (label-end (match-end 1))
             (url-beg (match-beginning 2))
             (url-end (match-end 2)))
        (make-button label-beg label-end
                     'type 'lui-button
                     'action 'lui-button-activate
                     'lui-button-function 'browse-url
                     'lui-button-arguments (list (buffer-substring-no-properties
                                                  url-beg url-end )))
        (goto-char (1- label-beg))
        (delete-char 1)
        (delete-region (1- label-end) url-end))))

  ;; attach add-link to the mrkdwn parser
  (advice-add 'slack-mrkdwn-add-face :after #'ii/slack-mrkdwn-add-link)

  (defun ii/lui-add-link (start end)
    "Add a markdown link to a text buffer.

  If the region is active and starts with 'http' use the region as
  the URL and query for a link label; if the region is active and
  does not start with 'http' use the region as the link label and
  query for a URL.  Prompt for both if the region is not active."
    (interactive "r")
    (flet ((link (label url)
                 (if (use-region-p) (delete-region start end))
                 (insert (format "[%s](%s) " label url))))
      ;; no region specified
      (if (not (use-region-p))
          (let ((url (read-from-minibuffer "URL: "))
                (label (read-from-minibuffer "Label: ")))
            (link label url))
        (let ((text (buffer-substring-no-properties start end)))
          ;; the region is an HTTP url
          (if (string= (substring text 0 4) "http")
              (let ((label (read-from-minibuffer "Label: ")))
                (link label text))
            ;; the region is the label
            (let ((url (read-from-minibuffer "URL: ")))
              (link text url)))))))

  (define-key lui-mode-map (kbd "M-k") 'ii/lui-add-link))
;; End of Slack configuration

;; org-mode configuation
(use-package org-journal
  :ensure t
  :config
  (setq
   org-journal-carryover-items nil
   org-journal-dir "~/CloudDocs/journal/"
   org-journal-file-type 'weekly
   org-journal-file-format "%Y/%m-%d"
   org-journal-start-on-weekday 7))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c i") 'org-journal-new-entry)
(global-set-key (kbd "C-c o") 'org-clock-out)
(global-set-key (kbd "C-c j") 'org-journal-open-current-journal-file)
(global-set-key (kbd "C-c C-i") 'org-clock-goto)

(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:blank:]]*$")))

(defun ii/clockreport ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (not (current-line-empty-p))
        (progn
          (insert "\n")
          (goto-char (point-min))))
    (org-clock-report 1)))

(define-key org-mode-map (kbd "C-c C-q") 'bury-buffer)
(define-key org-mode-map (kbd "C-c C-r") 'ii/clockreport)

;; C-c C-x C-j: open last clocked task

(add-hook 'org-journal-after-entry-create-hook 'org-clock-in)
(add-hook 'ii/clockreport 'org-clock-out-hook)

;; These things seem to be affected by nano...?
(setq
 mac-use-title-bar t
 initial-major-mode 'elisp-lisp-mode
 completion-styles '(orderless)
 completion-category-defaults nil
 completion-category-overrides '((file (styles partial-completion))))


(defun ii/print-hash-table (h)
  "String representation of a hash table H"
  (maphash (lambda (k v)
             (insert (format "%s: %s\n" k v))) h))

(use-package eshell-vterm
  :ensure t
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode))

(add-hook
 'eshell-mode-hook
 (lambda ()
   (eshell/alias "ff" "find-file $1")
   (eshell/alias "d" "dired $1")
   (eshell/alias "less" "find-file-read-only $1")))

(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo))

(use-package eshell-fringe-status
  :ensure t
  :config
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

;; Eshell prompt configuration.
(defface ii/eshell-branch-face
  '((t (:foreground "#008b8b")))
  "Git branch face.")

(defface ii/eshell-untracked-face
  '((t (:foreground "#fff68f")))
  "Git untracked file marker face.")

(defface ii/eshell-changed-face
  '((t (:foreground "#fff68f")))
  "Git modified file marker face.")

(defface ii/eshell-project-face
  '((t (:foreground "#008b8b")))
  "Project name face.")

(defface ii/eshell-project-path-face
  '((t (:foreground "#90ee90")))
  "Partial path face.")

(defvar ii/shell-arrow "❯")
(defvar ii/shell-middot "·")

(setq eshell-prompt-function #'ii/eshell-custom-prompt)
(setq eshell-prompt-regexp
      (concat "^[^#$" ii/shell-arrow "]* [$#" ii/shell-arrow "] "))

(defun ii/eshell-custom-prompt ()
  (interactive)
  (let ((prj-current (project-current nil)))
    (if prj-current
        (ii/eshell-custom-prompt-project prj-current)
      (ii/eshell-custom-prompt-no-project))))

(defun ii/eshell-custom-prompt-project (prj)
  (interactive)
  (let* ((prj-directory (project-root prj))
         (prj-name (project-name prj))
         (short-dir (abbreviate-file-name (eshell/pwd)))
         (git-untracked (magit-untracked-files))
         (git-branch (magit-get-current-branch))
         (git-changed (magit-git-lines "diff" "--name-status"))
         (git-untracked-marker (if git-untracked " Ø" ""))
         (git-changed-marker (if git-changed " ∆" "")))
    (concat
     "\n"
     (propertize short-dir 'face 'nano-face-faded)
     " "
     (propertize git-branch 'face 'ii/eshell-branch-face)
     (propertize git-untracked-marker 'face 'ii/eshell-untracked-face)
     (propertize git-changed-marker 'face 'ii/eshell-changed-face)
     "\n"
     (ii/directory-with-project prj-directory prj-name (eshell/pwd))
     " "
     (propertize ii/shell-arrow 'face 'nano-face-faded)
     " ")))

(defun ii/eshell-custom-prompt-no-project ()
  (interactive)
  (concat (abbreviate-file-name (eshell/pwd))
          (if (= (user-uid) 0)
              " # "
            (concat " " ii/shell-arrow " "))))

(defun ii/directory-with-project (prj-directory prj-name pwd)
  ;; show the path relative to the project root
  (let* ((current-dir
          (if (s-starts-with-p "~" prj-directory)
              (abbreviate-file-name pwd) pwd))
         (dir-path
          (s-chop-prefix (s-chop-suffix "/" prj-directory)
                         current-dir))
         (dir (if (= (length dir-path) 0) "/" dir-path)))
    (concat
     (propertize prj-name 'face 'ii/eshell-project-face)
     " " (propertize ii/shell-middot 'face 'nano-face-faded)  " "
     (propertize dir 'face 'ii/eshell-project-path-face))))

;; End of Eshell prompt configuration


;; Jeez.  Everything wants to override this!
(setq-default tab-width 2)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(connection-local-criteria-alist
   '(((:application tramp :machine "localhost")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp :machine "montuoke-2.attlocal.net")
      tramp-connection-local-darwin-ps-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)
     ((:application eshell)
      eshell-connection-default-profile)))
 '(connection-local-profile-alist
   '((tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))
     (eshell-connection-default-profile
      (eshell-path-env-list))))
 '(go-ts-mode-indent-offset 2)
 '(isearch-lazy-highlight 'all-windows)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages
   '(which-key auto-dim-other-buffers eshell-fringe-status eshell-vterm org-journal treesit-auto flycheck lsp-ui auto-package-update tree-sitter-langs treesit-langs corfu-popupinfo corfu-popup mode-compile elixir-mode deadgrep org-mac-link noflet org-mac-iCal corfu-doc all-the-icons-completion yaml-pro flymake-json outline-magic impatient-mode markdown slack backup smart-comment hydra ip4g erlang erlang-mode elm-mode elm ace-window elpy elfeed elfeeds switch-window url-util show-paren show-paren-mode parens eldocx fringe fringe-mode company company-mode lsp-headerline lsp-mode docker hl-todo web-mode detached vterm quick-buffer-switch forge orderless consult kind-icon corfu marginalia vertico avy yaml-mode json-mode markdown-mode magit))
 '(tab-bar-close-button-show nil)
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator))
 '(tab-bar-select-tab-modifiers '(hyper super)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#1E2430"))))
 '(aw-leading-char-face ((t (:foreground "#BF616A" :height 2.0))))
 '(isearch ((t (:foreground "black" :background "plum1" :inherit nano-face-strong))))
 '(lazy-highlight ((t (:foreground "black" :background "indian red" :inherit nano-face-subtle))))
 '(tab-bar ((t (:height 1.1 :inherit variable-pitch))))
 '(tab-bar-tab ((t (:inherit tab-bar))))
 '(tab-bar-tab-inactive ((t (:foreground "#677691" :inherit tab-bar-tab))))
 '(variable-pitch ((t (:background "#2E3440" :foreground "#ECEFF4" :height 140 :family "Avenir Book")))))
(put 'downcase-region 'disabled nil)
