;; Bootstrap straight.el
;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; MacOS specific configuration
(if (eql system-type 'darwin)
    (setq mac-option-modifier '(:function alt :mouse alt)
          mac-right-command-modifier 'super
          mac-right-option-modifier 'hyper
          ns-alternate-modifier 'super
          ns-command-modifier 'meta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NANO Setup
;;
;; Color reference: https://www.nordtheme.com/docs/colors-and-palettes
(straight-use-package
 '(nano-emacs
   :type git
   :host github
   :repo "rougier/nano-emacs"))

(setq nano-font-family-monospaced "Monaco")
(setq nano-font-family-proportional "Arial")
(setq nano-font-size 12)

(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)
(nano-theme-set-dark)
(setq nano-color-background "#232831")
(call-interactively 'nano-refresh-theme)
(require 'nano-defaults)
(require 'nano-bindings)
(require 'nano-session)

(straight-use-package 'nano-modeline)
(require 'nano-modeline)
(nano-modeline-prog-mode t)

(require 'nano-colors)
(set-face-background 'nano-modeline-active
                     (nord-color "aurora-0"))
(set-face-background 'nano-modeline-inactive
                     (nord-color "polar-night-1"))
(set-face-background 'nano-modeline-status
                     (nord-color "snow-storm-0"))

(defun ii/nano-modeline-vterm-mode ()
  "Nano line for vterm mode"
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status ">_") " "
             (ii/nano-modeline-rename-emulator-buffer))
           '((nano-modeline-default-directory 16) " "
             (nano-modeline-window-dedicated))))
(add-hook 'vterm-mode-hook #'ii/nano-modeline-vterm-mode)
(add-hook 'eshell-mode-hook #'ii/nano-modeline-vterm-mode)

(defun ii/nano-modeline-rename-emulator-buffer ()
  (let* ((prj (ii/project-current-short-name))
         (mode (s-chop-suffix "-mode" (format "%s" major-mode)))
         (dir (nano-modeline-default-directory 32))
         (name (if prj
                   (format "%s: %s" mode prj)
                 (format "%s: %s" mode dir))))
    (with-current-buffer (rename-buffer name t))
    (propertize name 'face nano-modeline-base-face)))

(defun ii/nano-modeline-crdt-mode ()
  "Nano line for CRDT mode"
  (funcall nano-modeline-position
           '((nano-modeline-buffer-status "@@") " "
             (nano-modeline-buffer-name))
           '((nano-modeline-cursor-position)
             (nano-modeline-window-dedicated))))
(add-hook 'crdt-mode-hook #'ii/nano-modeline-crdt-mode)

(defun ii/nano-modeline-window-zoom (dedicated-symbol)
  "Advice after `nano-modeline-window-dedicated'."
  (if (and (fboundp 'zoom-window--enable-p)
           (zoom-window--enable-p))
      (concat (propertize "🔍  " 'face (nano-modeline-face 'secondary))
              dedicated-symbol)
    dedicated-symbol))

(advice-add #'nano-modeline-window-dedicated
            :filter-return
            'ii/nano-modeline-window-zoom)

;; End of NANO Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :bind (("C-M-SPC" . cycle-spacing)
	     ("<f5>" . scratch-buffer)
	     ("C-+" . text-scale-increase)
	     ("C--" . text-scale-decrease)
	     ("C-=" . ii/text-scale-reset)
	     ("C-c w" . display-fill-column-indicator-mode))

  :hook ((after-save . executable-make-buffer-file-executable-if-script-p))


  :config
  (setq
   byte-compile-warnings '(not obsolete)
   warning-suppress-log-types '((comp) (bytecomp))
   warning-minimum-level :error
   frame-title-format "%b"
   font-lock-maximum-decoration t
   mac-use-title-bar t
   default-directory "~/"
   widget-image-enable nil
   tab-width 2

   ;; fill column
   display-fill-column-indicator-character 124
   display-fill-column-indicator-column 80

   initial-major-mode 'emacs-lisp-mode
   completion-styles '(orderless)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))

   calendar-latitude 29.9510
   calendar-longitude -90.0715)

  (midnight-mode)
  (subword-mode)
  (undelete-frame-mode)
  (pixel-scroll-precision-mode)
  (add-to-list 'completion-ignored-extensions ".#")
  (global-goto-address-mode)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")

  ;; Don't warn me about these...
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)

  ;; Always backspace, never erase
  (global-unset-key (kbd "<S-delete>"))
  (normal-erase-is-backspace-mode 0)

  ;; Construct a reasonable exec path
  (defun ii/emacs-dir-file (file)
    "Concatenate FILE to user-emacs-directory."
    (expand-file-name file user-emacs-directory))

  (defun ii/home-dir-file (file)
    "Concatenate FILE to $HOME."
    (expand-file-name file (getenv "HOME")))

  (setq ii/exec-path
	    `("/usr/local/bin"
	      "/opt/homebrew/bin"
	      ,(ii/home-dir-file "go/bin")))
  (mapc (lambda (path) (add-to-list 'exec-path path)) ii/exec-path)

  (defun ii/text-scale-reset ()
    "Reset the text scale to zero."
    (interactive)
    (text-scale-set 0))

  ;; Ersatz key bindings
  ;;
  ;; These define-key statements translate (kdb ...) to new key
  ;; presses.  We do this to override (with a big hammer) any modes
  ;; that try to change bindings we want available everywhere.  VTerm
  ;; is especially egregious about this.

  ;; (see ace-window)
  (define-key key-translation-map (kbd "C-<return>") [ersatz-c-return])

  ;; (see zoom-window)
  (define-key key-translation-map (kbd "M-z") [ersatz-m-z])

  ;; (see detached)
  (define-key key-translation-map (kbd "<f2>") [ersatz-f2])

  ;; (see ii/close-help-window)
  (define-key key-translation-map (kbd "C-z") [ersatz-c-z])

  ;;
  ;; Quit emacs.  C-x C-c is rebound below to a more often used fn.  I
  ;; don't quit Emacs all that often and never use zap-to-char, so
  ;; here we are.
  (global-set-key (kbd "C-x <ersatz-c-z>") 'save-buffers-kill-terminal)

  ;; Windowing
  (setq switch-to-buffer-obey-display-actions t
	    switch-to-buffer-in-dedicated-window 'pop)

  (setq display-buffer-alist
	    `((,(rx (or "vterm"
		            "VTerm"))
	       (display-buffer-reuse-window))

	      (,(rx (or "*detached shell command*"
		            "*detached-session-output"
		            "cmd: " ;; for specially named detached shell commands
		            "*detached-list*"
		            "*Flycheck errors*"))
	       (display-buffer-in-side-window)
	       (side . bottom)
	       (slot . 0)
	       (window-height . 15))

	      (,(rx (or "*help*"
		            "*info*"))
	       (display-buffer-reuse-window
	        display-buffer-in-side-window)
	       (side . right)
	       (slot . 0)
	       (window-width . 80))

	      (,(rx (or "*deadgrep"
		            "*xref*"
		            "Magit"
		            "converge.org"
		            "COMMIT_EDITMSG"))
	       (display-buffer-in-side-window)
	       (side . left)
	       (slot . 0)
	       (window-width . 80)
	       (window-parameters
	        (no-delete-other-windows . t)))))

  (defun ii/close-help-window ()
    "Cloas all *Help* windows."
    (interactive)
    (dolist (win (window-list))
      (if (equal "*Help*" (buffer-name (window-buffer win)))
	      (delete-window win))))
  (global-set-key [ersatz-c-z] 'ii/close-help-window)

  ;; Some backup magic.  I hate losing things.
  (setq vc-make-backup-files t)
  (defun ii/save-buffer-force-backup (arg)
    "Force a backupp on each save-buffer."
    (interactive "P")
    (if (consp arg) (save-buffer) (save-buffer 16)))
  (global-set-key [remap save-buffer] 'ii/save-buffer-force-backup)
  (global-set-key (kbd "C-x s") 'ii/save-buffer-force-backup)

  ;;
  ;; From https://www.emacswiki.org/emacs/FindFileAtPoint
  ;;
  (defvar ffap-file-at-point-line-number nil
    "Variable to hold line number from the last `ffap-file-at-point' call.")

  (defadvice ffap-file-at-point (after ffap-store-line-number activate)
    "Search `ffap-string-at-point' for a line number pattern and
save it in `ffap-file-at-point-line-number' variable."
    (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
           (name
            (or (condition-case nil
                    (and (not (string-match "//" string)) ; foo.com://bar
			             (substitute-in-file-name string))
                  (error nil))
		        string))
           (line-number-string
            (and (string-match ":[0-9]+" name)
		         (substring name (1+ (match-beginning 0)) (match-end 0))))
           (line-number
            (and line-number-string
		         (string-to-number line-number-string))))
      (if (and line-number (> line-number 0))
          (setq ffap-file-at-point-line-number line-number)
	    (setq ffap-file-at-point-line-number nil))))

  (defadvice find-file-at-point (after ffap-goto-line-number activate)
    "If `ffap-file-at-point-line-number' is non-nil goto this line."
    (when ffap-file-at-point-line-number
      (goto-line ffap-file-at-point-line-number)
      (setq ffap-file-at-point-line-number nil)))
  ;;
  ;; End of ffap advice
  ;;


  ;;
  ;; font-lock tweaks
  ;;
  (set-face-attribute 'font-lock-function-name-face nil
                      :height 1.1
                      :foreground (nord-color "aurora-2"))
  (set-face-attribute 'font-lock-string-face nil
                      :foreground (nord-color "aurora-3"))
  (set-face-attribute 'font-lock-constant-face nil
                      :foreground (nord-color "frost-0"))
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground (nord-color "frost-1"))
  (set-face-attribute 'font-lock-type-face nil
                      :foreground (nord-color "aurora-4"))

  ;; C-= to reset text scaling
  (global-set-key (kbd "C-=")
                  (lambda () (interactive) (text-scale-set 0)))

  ) ;;; end (use-package emacs ...


(use-package tab-bar
  :init
  (setq tab-bar-select-tab-modifiers '(super))
  :config
  (setq tab-bar-close-button-show nil
        tab-bar-format '(" " tab-bar-format-history
                         tab-bar-format-tabs-groups))
  (set-face-attribute 'tab-bar nil
                      :height 1.3
                      :inherit 'variable-pitch
                      :foreground (nord-color "snow-storm-0")
                      :background (nord-color "polar-night-2"))
  (set-face-attribute 'tab-bar-tab-inactive nil
                      :foreground (nord-color "polar-night-1")
                      :background (nord-color "polar-night-2")
                      :inherit 'tab-bar))


(use-package view
  :bind (("<f3>" . view-mode))
  :init
  ;; When finding functions with M-x find-function, turn view mode on
  ;; to prevent accidentally modifying the library.
  (add-hook 'find-function-afer-hook #'view-mode-enter)
  :config
  ;; Enable view mode for all buffers visiting read-only files.
  (setq view-read-only t))


;; NB: requires NANO
(use-package ace-window
  :straight t
  :custom
  (aw-keys '(49 50 51 52 53 54 55 56 57))
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground (nord-color "aurora-0")
                      :height 2.0)
  :bind (("M-o" . ace-window)
	     ([ersatz-c-return] . ace-window)))

(use-package zoom-window
  :straight t
  :bind (("M-z" . zoom-window-zoom)
	     ([ersatz-m-z] . zoom-window-zoom))
  :config
  (defun ii/enlarge-on-zoom (&rest r)
    "When zooming a window, enlarge the text; reverse the
 modification when the window is un-zoomed."
    (if (zoom-window--enable-p)
	    (text-scale-set 2)
      (text-scale-set 0)))
  (advice-add #'zoom-window-zoom :after #'ii/enlarge-on-zoom))

(use-package dired
  :config
  (set-face-attribute 'dired-directory nil
                      :foreground (nord-color "aurora-2"))
  (setq dired-use-ls-dired nil
        dired-vc-rename-file t))

(use-package all-the-icons
  :straight t
  :config
  ;; Use 'prepend for the NS and Mac ports or Emacs will crash.
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'prepend)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'prepend))

(use-package all-the-icons-completion
  :straight t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package ibuffer
  :straight t
  :config
  (setq ibuffer-default-sorting-mode 'major-mode)
  :bind ([remap list-buffers] . ibuffer))

(use-package url
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

(use-package paren
  :config
  (show-paren-mode t)
  (setq show-paren-style 'expression
        show-paren-context-when-offscreen 'overlay
        show-paren-priority 9999))

(use-package deadgrep
  :straight t
  :bind ("H-g" . deadgrep))

(use-package hydra
  :straight t
  :demand t)

(use-package crdt
  :straight t
  :after hydra
  :bind ("s-p" . ii/crdt/body)
  :config
  (defhydra ii/crdt (:color pink
			                :hint nil
			                :exit t)
    "
CRDT Actions

_s_: share buffer         _c_: connect
_b_: switch buffers       _l_: list buffers
_f_: follow user          _L_: list sessions
_F_: stop follow user
_v_: visualize mode       _D_: disconnect
                        _X_: stop session
                        _S_: stop sharing buffer
"
    ("s" crdt-share-buffer)
    ("b" crdt-switch-to-buffer)
    ("f" crdt-follow-user)
    ("F" crdt-stop-follow)
    ("v" crdt-visualize-author-mode)
    ("c" crdt-connect)
    ("l" crdt-list-buffers)
    ("L" crdt-list-sessions)
    ("D" crdt-disconnect)
    ("X" crdt-stop-session)
    ("S" crdt-stop-share-buffer)
    ("q" nil "quit" :color build)))


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
  :config
  (info-initialize))


(use-package devdocs
  :straight t
  :bind ("C-h C-j" . devdocs-lookup))


;;
;; Completion
;;
;; Vertico
;; see https://github.com/emacs-straight/vertico
(use-package vertico
  :straight t
  :bind (:map vertico-map
              (("<backspace>"   . vertico-directory-delete-char)
               ("C-w"           . vertico-directory-delete-word)
               ("C-<backspace>" . vertico-directory-delete-word)))
  :hook
  ((rfn-eshadow-update-overlay . vertico-directory-tidy))
  :custom
  (vertico-count 14)
  (vertico-resize nil)
  (vertico-cycle nil)
  :init
  ;; For some reason vertico-directory is not in the autoload path
  ;; correctly (the bindings above require it).  Here we add
  ;; vertico/extensions to the load-path and manually require the
  ;; library.
  (let* ((dir (file-name-directory (locate-library "vertico")))
         (base (expand-file-name "extensions" dir)))
    (add-to-list'load-path base))
  (require 'vertico-directory)

  (fido-mode -1)
  (vertico-mode))

;; Marginalia
(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

;; Corfu
;;
;; see https://github.com/minad/corfu

(use-package corfu
  :straight t
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
  (set-face-attribute 'corfu-default nil
                      :background (nord-color "polar-night-0")
                      :foreground (nord-color "aurora-3"))
  (set-face-attribute 'corfu-current nil
                      :background (nord-color "frost-3")
                      :foreground (nord-color "snow-storm-1"))
  (set-face-attribute 'corfu-annotations nil
                      :foreground (nord-color "snow-storm-0"))
  (global-corfu-mode))

(use-package corfu-popupinfo
  :custom
  (corfu-popupinfo-min-width 40)
  (corfu-popupinfo-max-width 90)
  (corfu-popupinfo-min-height 2)
  (corfu-popupinfo-delay '(2.0 . 1.0))
  (corfu-popupinfo-hide t)
  :bind
  (("M-n" . corfu-popupinfo-scroll-up)
   ("M-p" . corfu-popupinfo-scroll-down))
  :config
  (corfu-popupinfo-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package consult
  :straight t
  :bind (("C-x b" . consult-buffer)
         ("M-g M-g" . consult-goto-line)
         ("M-i" . consult-imenu)
         ("M-s l" . consult-line)
         ("M-s d" . consult-find)
         ("C-x r b" . consult-bookmark)
         ("C-c m" . consult-mode-command)
         ("M-y" . consult-yank-pop)))


(use-package orderless
  :straight t)

(use-package json-mode
  :after flymake-json
  :straight t)

(use-package magit
  :straight t
  :demand t
  :bind (("s-g" . 'magit-status))
  :hook (before-save . magit-wip-commit-initial-backup)
  :config
  (setq magit-commit-show-diff nil)
  (magit-wip-mode 1))

(use-package ghub
  :straight t)

(use-package git-gutter
  :straight t
  :config
  (global-git-gutter-mode +1))

(use-package yaml-mode
  :straight t)

(use-package vterm
  :straight t
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
  :straight t)

(use-package quick-buffer-switch
  :straight t
  :config
  (qbs-init)
  (qbs-add-predicates
   (make-qbs:predicate
    :name 'vterm
    :shortcut "C-v"
    :test '(when (member major-mode '(vterm-mode eshell-mode))
             qbs:buffer-name))))

(use-package whitespace
  :straight t
  :config
  (setq whitespace-style '(trailing tabs))
  (global-whitespace-mode t)
  :hook (before-save . delete-trailing-whitespace))

(setq nano-modeline-mode-formats nil)
(use-package detached
  :straight t
  :init
  (detached-init)
  (setq comint-scroll-show-maximum-output t)
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ("C-M-<return>" . ffap)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :config
  (global-set-key [ersatz-f2] 'detached-list-sessions)
  (if (eq system-type 'darwin)
      (setq detached-notification-function #'detached-extra-alert-notification))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type)))

(use-package cperl-mode
  :straight
  :config
  (set-face-attribute 'cperl-hash-face nil
                      :slant 'unspecified
                      :foreground (nord-color "aurora-1"))
  (defalias 'perl-mode 'cperl-mode)
  (setq cperl-invalid-face nil)
  (add-to-list 'auto-mode-alist '("\\.t$"  . cperl-mode))
  (add-to-list 'auto-mode-alist '("\\.p[lm]" . cperl-mode)))


(use-package web-mode
  :straight t
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

(use-package hl-todo
  :straight t
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

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode 1))

(use-package docker
  :straight t
  :bind (("C-c C-d" . docker)))

(use-package flymake
  :straight t
  :bind ("<f1>" . flycheck-list-errors)
  :config
  (set-face-attribute 'error nil
                      :background 'unspecified
                      :foreground (nord-color "aurora-0"))
  )

(use-package eldoc
  :custom
  (eldoc-echo-area-prefer-doc-buffer t))

(use-package project
  :config
  (defun ii/project-current-short-name ()
    "A short name for the current project or nil."
    (interactive)
    (let ((current (project-current)))
      (if current
          (let* ((name (caddr current))
                 (prj (cadr (reverse (file-name-split name)))))
            prj)
        nil))))


;; LSP-Mode
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "s-l")
  (defun ii/lsp-mode-setup-completion()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :bind (("<f15>" . lsp-format-buffer)
         ("<f14>" . lsp-find-references))
  :hook ((go-mode . lsp)
         (go-ts-mode . lsp)
         (elixir-mode . lsp)
         (python-mode . lsp)
         (lsp-completion-mode . ii/lsp-mode-setup-completion))
  :custom
  (lsp-completion-provider :none)
  (lsp-modeline-diagnostics-enable nil)
  :config
  ;; (lsp-register-custom-settings
  ;;  '(("gopls.completeUnimported" t t)
  ;;    ("gopls.staticcheck" t t)))
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

(use-package lsp-ui
  :straight t
  :custom
  (lsp-ui-peek-enable t))

(use-package flycheck
  :straight t)

(use-package treesit-auto
  :straight t
  :demand t
  :config
  (setq treesit-auto-install t)
  (global-treesit-auto-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (setq global-tree-sitter-mode t))

(use-package go-ts-mode
  :straight t
  :demand t
  :after (lsp-mode)
  :config
  (defun ii/lsp-go-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (defun ii/go-debug-var ()
    "Write a printf statement for the variable at point."
    (interactive)
    (let* ((var (thing-at-point 'word 'no-properties))
           (txt (concat "fmt.Printf(\"\\n** " var ": %+v\\n\", " var ")")))
      (end-of-line)
      (insert "\n")
      (indent-for-tab-command)
      (insert txt "\n")
      (kill-new txt)))

  (defun ii/go-struct-member ()
    "Take the tedium out of these specifications."
    (interactive)
    (let* ((slot (read-string "Slot Name: "))
           (type (read-string "Type: " "string"))
           (yaml (read-string "YAML: "))
           (json (read-string "JSON: " yaml))
           (csv  (read-string "CSV: "))
           (table (read-string "Table: ")))
      (insert
       slot " " type
       " `yaml:\"" yaml
       "\" json:\"" json
       "\" csv:\"" csv
       "\" table:\"" table "\"`\n")))

  :custom
  (go-ts-mode-indent-offset 2)
  :bind (("H-d" . ii/go-debug-var))
  :hook ((go-mode . tree-sitter-hl-mode)
         (go-mode . ii/lsp-go-save-hooks)))

;; Go REPL
;;
;; go install github.com/x-motemen/gore/cmd/gore@latest
(use-package gorepl-mode
  :straight t)

;; Python
;;
;; Requires
;;  pip3 install jedi autopep8 flake8 ipython importmagic yapf
(use-package elpy
  :straight t
  :config
  (elpy-enable)
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "-i --simple-prompt"))

;; Elm
(use-package elm-mode
  :straight t
  :config
  (setq elm-indent-after-keywords
        '(("of" 2) ("in" 2 0) ("{" 2) "if" "then" "else" "let")
        elm-indent-offset 2
        elm-sort-imports-on-save t))

;; Erlang
(use-package erlang
  :straight t
  :config
  (setq erlang-check-module-name t
        erlang-indent-level 2)
  (defun ii/set-erlang-indent-level (spaces)
    "Change the Erlang indentation level."
    (interactive "nIndention Level: ")
    (set-variable 'erlang-indent-level spaces t)))

;; Elixir
(use-package elixir-mode
  :straight t)

;; Tramp
(use-package tramp
  :config
  (setq tramp-shell-prompt-pattern
        "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>❯] *\\(\\[[[:digit:];]*[[:alpha:]] *\\)*"))

(use-package smart-comment
  :straight t
  :config
  :bind ([remap comment-dwim] . smart-comment))

(use-package server
  :config
  (unless (server-running-p) (server-start)))

(use-package markdown-mode
  :straight t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . gfm-mode)))

;; Restclient
(use-package restclient
  :straight t
  :mode "\\.rest\\'")

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'osx-notifier))

(use-package sql
  :straight t
  :mode "\\.eqlite\\'"
  :config
  (setq sql-product 'postgres
        sql-mysql-options '("--protocol=tcp")))

(use-package outline-magic
  :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package open-junk-file
  :straight t
  :config

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

  (global-set-key (kbd "C-x j") 'open-junk-file)
  (global-set-key (kbd "C-x C-j") 'ii/open-current-junk-directory)
  (global-set-key (kbd "C-x M-j") 'ii/rgrep-junk-directory))

(use-package fringe
  :config
  (setq indicate-buffer-boundaries 'left
        indicate-empty-lines t
        x-underline-at-descent-line t
        global-linum-mode nil)
  (setq-default left-fringe-width 15)
  (window-divider-mode 0))


(use-package eshell
  :bind ("s-n" . eshell)
  :demand t
  :after corfu
  :init (require 'eshell)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "ff" "find-file $1")
              (eshell/alias "d" "dired $1")
              (eshell/alias "less" "find-file-read-only $1")))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  (defun eshell/gst (&rest args)
    (magit-status (pop args) nil)
    (eshell/echo))

  ;; Eshell prompt configuration.
  (defface ii/eshell-branch-face
    `((t (:foreground ,(nord-color "frost-0"))))
    "Git branch face.")

  (defface ii/eshell-untracked-face
    `((t (:foreground ,(nord-color "aurora-2"))))
    "Git untracked file marker face.")

  (defface ii/eshell-changed-face
    `((t (:foreground ,(nord-color "aurora-2"))))
    "Git modified file marker face.")

  (defface ii/eshell-project-face
    `((t (:foreground ,(nord-color "aurora-4"))))
    "Project name face.")

  (defface ii/eshell-project-path-face
    `((t (:foreground ,(nord-color "aurora-4"))))
    "Partial path face.")

  (defface ii/eshell-path-face
    `((t (:foreground ,(nord-color "frost-3"))))
    "Full path face.")

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
       (propertize short-dir 'face 'ii/eshell-path-face)
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
    (let ((prompt (concat
                   (abbreviate-file-name (eshell/pwd))
                   (if (= (user-uid) 0)
                       " # "
                     (concat " " ii/shell-arrow " ")))))
      (propertize prompt 'face 'ii/eshell-path-face)))


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

  (with-eval-after-load 'em-ls
    (set-face-attribute 'eshell-ls-directory nil
                        :inherit 'default
                        :foreground (nord-color "aurora-2"))
    (set-face-attribute 'eshell-ls-executable nil
                        :foreground (nord-color "aurora-3")))


  ) ;; end of use-package eshell


(use-package eshell-vterm
  :straight t
  :demand t
  :after eshell
  :config
  (eshell-vterm-mode))

(use-package eshell-fringe-status
  :straight t
  :after eshell
  :config
  (set-face-attribute 'eshell-fringe-status-success nil
                      :foreground (nord-color "aurora-3"))
  (set-face-attribute 'eshell-fringe-status-failure nil
                      :foreground (nord-color "aurora-0"))
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

;; IP4G (local development toolkit)
(let ((ip4g-dir
       (expand-file-name "~/projects/converge/morpho-utils/emacs")))
  (if (file-directory-p ip4g-dir)
      (progn
        (add-to-list 'load-path ip4g-dir)
        (require 'ip4g)
        (global-set-key (kbd "s-c") #'ip4g/current-pcloud)
        (global-set-key (kbd "s-i") #'ip4g/hydra/body))))

(use-package ace-kill
  :after hydra
  :straight '(ace-kill :type git
                       :host github
                       :repo "bunnylushington/ace-kill")
  :bind ("s-u" . 'ace-kill-hydra/body))


(use-package org
  :config
  (add-hook 'org-mode-hook #'nano-modeline-org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Slack
;;
;; NB: a file, ~/.emacs-slack-config, should set the values of
;; ii/slack-current-team, ii/slack-team-name, and
;; ii/slack-subscribed-channels.

(use-package slack
  :straight t
  :if (file-exists-p (ii/home-dir-file ".emacs-slack-config"))
  :after hydra
  :bind   ("s-s" . slack-mode-hydra/body)
  :config
  (load-file (ii/home-dir-file ".emacs-slack-config"))
  (slack-register-team
   :name ii/slack-team-name
   :default t
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
   :full-and-display-names t
   :visible-threads t
   :websocket-event-log-enabled nil
   :modeline-enable nil
   :subscribed-channels ii/slack-subscribed-channels)
  (slack-start)
  (define-key lui-mode-map (kbd "<return>") 'newline)
  (define-key lui-mode-map (kbd "M-<return>") 'lui-send-input)
  (setq
   lui-fill-column 78
   lui-time-stamp-position nil
   lui-flyspell-p nil
   lui-fill-type nil
   slack-log-level 'error
   slack-buffer-emojify 't
   slack-display-team-name nil
   slack-enable-wysiwyg 't
   slack-file-dir "~/Downloads"
   slack-mrkdwn-blockquote-sign ""
   slack-buffer-create-on-notify 't
   slack-prefer-current-team 't
   slack-render-image-p nil
   slack-thread-also-send-to-room nil)

  ;; this method always writes to the trace buffer which grows without
  ;; bound.  redefine the fn so it's more sane.
  (with-eval-after-load 'slack-log
    ;; Essentially no-op.  I think these data are pretty much captured
    ;; by slack-log.
    (defun slack-log-websocket-payload (payload team &optional out) t)

    ;; Use the user-level to determine if messages should be logged
    (cl-defun slack-log (msg team &key
                             (logger #'slack-message-logger)
                             (level 'debug))
      "LEVEL is one of 'trace, 'debug, 'info, 'warn, 'error"
      (let ((log (format "%s [%s] %s - %s"
                         (format-time-string slack-log-time-format)
                         level
                         msg
                         (slack-team-name team)))
            (buf (get-buffer-create (slack-log-buffer-name team))))
        (when (functionp logger)
          (funcall logger msg level team))
        (let ((user-level (slack-log-level-to-int slack-log-level))
              (current-level (slack-log-level-to-int level)))
          (when (<= current-level user-level)
            (with-current-buffer buf
              (setq buffer-read-only nil)
              (save-excursion
                (goto-char (point-max))
                (insert log)
                (insert "\n"))
              (setq buffer-read-only t)))))))


  (defun ii/lui-setup ()
    (setq fringes-outside-margins nil
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
    (cl-flet ((link (label url)
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

(defun ii/print-hash-table (h)
  "String representation of a hash table H"
  (maphash (lambda (k v)
             (insert (format "%s: %s\n" k v))) h))

(defun ii/decode-jwt (start end &optional jwt)
  "Decode JWT in region and print to help buffer."
  (interactive "r")
  (let* ((tok (if jwt jwt
                (buffer-substring start end)))
         (data (s-split "\\." tok))
         (header (car data))
         (claims (cadr data)))
    (with-temp-buffer
      (insert (format "%s\n\n%s"
                      (base64-decode-string header t)
                      (base64-decode-string claims t)))
      (json-pretty-print-buffer)
      (with-output-to-temp-buffer "*JWT*"
        (princ (buffer-string)))))
  t)

;; GCP Access Token hack
(defun ii/verify-gcp-access-token (start end &optional tok)
  "Verify a GCP access token"
  (interactive "r")
  (cl-flet ((output-fn
              (cl-function (lambda (&key data &allow-other-keys)
                             (with-output-to-temp-buffer "*GCP Token*"
                               (princ data))))))
    (let* ((sub (if tok tok (buffer-substring start end)))
           (token (replace-regexp-in-string "\n" "" sub)))
      (message token)
      (request
       "https://www.googleapis.com/oauth2/v1/tokeninfo"
       :params `(("access_token" . ,token))
       :error #'output-fn
       :success #'output-fn)
      t)))




;; This audio control stuff requires:
;;   brew install switchaudio-osx
;;   brew tap meowmeowmeowcat/taps
;;   brew install meowmeowmeowcat/taps/volume
(defun ii/audio-output ()
  (interactive)
  (let* ((sources (ii/audio-all-outputs))
         (new-source (completing-read "New source: " sources)))
    (ii/audio-set-output new-source)))

(defun ii/audio-set-output (source)
  (shell-command
   (format "SwitchAudioSource -s \"%s\"" source)))

(defun ii/audio-volume (&optional volume)
  (interactive "sVolume: ")
  (shell-command (concat "volume " volume)))

(defun ii/audio-all-outputs ()
  (let ((outputs ())
        (source-string (shell-command-to-string
                        "SwitchAudioSource -a -t output -f cli")))
    (dolist (src (split-string source-string "\n" t " "))
      (let ((record (split-string src ",")))
        (add-to-list 'outputs (car record))))
    outputs))
