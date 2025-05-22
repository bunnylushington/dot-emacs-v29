(defvar exrepl-current-buffer nil)
(defvar exrepl-prompt-pattern "iex(%s)> ")

;; iex(19)> c("/tmp/foo/ex.ex")

(defun exrepl-generic-prompt-pattern ()
  (format exrepl-prompt-pattern "[0-9]+"))

(defun exrepl-specific-prompt-pattern (nth)
  (format exrepl-prompt-pattern nth))

(defun exrepl-copy-last-output ()
  (interactive)
  (with-current-buffer
      exrepl-current-buffer 'action nil
      (save-excursion
        (vterm-copy-mode 1)
        (goto-char (point-max))
        (previous-line)
        (let (beg str (end (line-end-position)))
          (re-search-backward (exrepl-generic-prompt-pattern))
          (next-line)
          (setq str (buffer-substring (line-beginning-position) end))
          (kill-new str)
          (vterm-copy-mode -1)))))

(defun exrepl-copy-some-output (&optional n)
  (interactive "nHistory #: ")
  (let ((pat (exrepl-specific-prompt-pattern n)))
    (with-current-buffer
        exrepl-current-buffer 'action nil
        (save-excursion
          (vterm-copy-mode 1)
          (re-search-backward pat)
          (next-line)
          (let (end str (beg (line-beginning-position)))
            (re-search-forward (exrepl-generic-prompt-pattern))
            (setq str (buffer-substring beg (line-beginning-position)))
            (kill-new str)
            (vterm-copy-mode -1))))))

(defun exrepl-set-current-buffer (&optional buf)
  (interactive "bBuffer: ")
  (setq exrepl-current-buffer buf))

(defun exrepl-send-line (line &optional buf)
  (with-current-buffer
      exrepl-current-buffer 'action nil
      (if (string= major-mode "vterm-mode")
          (vterm-send-string (concat line "\n"))
        (message "Will only send to vterm mode buffers."))))

(defun exrepl-recompile (&optional buf)
  (interactive)
  (exrepl-send-line "recompile"))

(defun exrepl-send-current-region (&optional beg end)
  (interactive "r")
  (let ((reg (buffer-substring beg end)))
    (exrepl-send-line reg)))

(defun exrepl-find-enclosing-module ()
  (interactive)
  (re-search-backward
   "defmodule\s+\\([A-Z].+?\\)[,\s]" nil t)
  (let ((m (match-string 1)))
    (message m)
    m))

(defun exrepl-send-modularized-region (&optional beg end)
  (interactive "r")
  (let ((reg (buffer-substring beg end))
        (mod (exrepl-find-enclosing-module)))
    (exrepl-send-line
     (format "defmodule %s do\n%s\nend"
             (if mod mod "MyApp") reg))))

(defun exrepl-send-current-line ()
  (interactive)
  (let ((line (buffer-substring
               (line-beginning-position)
               (line-end-position))))
    (prin1 (substring-no-properties line))
    (exrepl-send-line (substring-no-properties line))))

(defun exrepl-compile-this-buffer-file ()
  (interactive)
  (save-buffer)
  (exrepl-compile-file (buffer-file-name)))

(defun exrepl-compile-file (&optional file)
  (interactive "fSource file: ")
  (let ((source (expand-file-name file)))
    (exrepl-send-line
     (format "c(\"%s\")" source))))
