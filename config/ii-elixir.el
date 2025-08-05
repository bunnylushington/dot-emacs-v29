(require 'treesit)
(require 'ansi-color)

(defun ii/elixir-prettify-spec ()
  (dolist (mapping
           (list
            '("|>" . "▷")
            '("->" . "➝")
            '("<-" . "⭠")
            '(">=" . "≧")
            '("<=" . "≦")
            '("@spec" . "🄢")
            '("@doc" . "🄓")
            '("@moduledoc" . "🄜")
            '("==" . "⩵")))
    (push mapping prettify-symbols-alist))
  (prettify-symbols-mode 1))

(defun elixir-ts--get-do-end-block-bounds ()
  "If point is on a 'do' or 'end' keyword, return the bounds of the block."
  (when (eq major-mode 'elixir-ts-mode)
    (let* ((node (treesit-node-at (point)))
           (node-type (and node (treesit-node-type node))))
      (when (and node-type (member node-type '("do" "end")))
        (when-let ((block-node (treesit-node-parent node)))
          (list (treesit-node-start block-node)
                (treesit-node-start block-node)
                (treesit-node-end block-node)
                (treesit-node-end block-node)))))))

(defun ii/elixir-show-paren-data-function ()
  "Custom `show-paren-data-function` for Elixir.
It checks for `do...end` blocks first, and falls back to the
default parenthesis/bracket matching otherwise."
  (or (elixir-ts--get-do-end-block-bounds)
      (show-paren--default)))

(with-eval-after-load 'paren
  (setq show-paren-data-function #'ii/elixir-show-paren-data-function))

(defun ii/elixir-font-lock-spec ()
  "Font-lock @spec as a comment."
  (let ((spec-rule
         (treesit-font-lock-rules
          :language 'elixir
          :feature 'ii-spec-as-comment
          :override t
          '((unary_operator
             operator: "@" @font-lock-preprocessor-face
             operand: (call
                       target: (identifier) @elixir-ts-comment-doc-identifier
                       (arguments (_) @font-lock-preprocessor-face))
             (:match "spec" @elixir-ts-comment-doc-identifier)))
          )))
    (setq-local treesit-font-lock-settings
                (append treesit-font-lock-settings spec-rule))))


;; Stolen from https://tinyurl.com/ycxucjue
;; via https://tinyurl.com/4f9am84x
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'endless/colorize-compilation)


(defun elixir-test-extras-credo ()
  (interactive)
  (elixir-test--run-test (vector "mix" "credo --strict" nil)))

(defun elixir-test-extras-doctor ()
  (interactive)
  (elixir-test--run-test (vector "mix" "doctor" nil)))

(defun elixir-test-extras-dialyzer ()
  (interactive)
  (elixir-test--run-test (vector "mix" "dialyzer" nil)))

(defun elixir-test-extras-coverage ()
  (interactive)
  (elixir-test--run-test (vector "mix" "testcov" nil)))


(defhydra ii/elixir-test (:color pink
                                 :hint nil
                                 :exit t)
  "
Elixir Test

_s_: test at point     _l_: rerun last test
_f_: test file         _u_: test parent directory
_d_: test directory    _._: rerun failed test
_a_: test all          _t_: toggle implementation/test

_c_: credo test        _y_: dialyzer
_o_: doctor test       _v_: coverage

"
  ("s" elixir-test-at-point)
  ("f" elixir-test-file)
  ("d" elixir-test-directory)
  ("a" elixir-test-all)
  ("l" elixir-test-rerun-last)
  ("u" elixir-test-up)
  ("." elixir-test-failed)
  ("t" elixir-test-toggle-implementation)
  ("c" elixir-test-extras-credo)
  ("o" elixir-test-extras-doctor)
  ("y" elixir-test-extras-dialyzer)
  ("v" elixir-test-extras-coverage)
  ("q" nil "quit" :color build))


(provide 'ii-elixir)
