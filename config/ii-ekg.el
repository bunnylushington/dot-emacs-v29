(use-package ekg
  :straight t
  :custom (ekg-display-note-template
           (concat "\n%n(titled)\n%n(text 500)\n%n(id)%n(tagged)%n(other)"
                   (make-string 40 ?_)))
  :config

  (set-face-attribute 'ekg-title nil
                      :foreground (nord-color "aurora-4")
                      :height 1
                      :underline nil)
  (set-face-attribute 'ekg-metadata nil
                      :background (nord-color "polar-night-0")
                      :foreground (nord-color "aurora-3")
                      :inherit 'ekg-tag)
  (set-face-attribute 'ekg-notes-mode-title nil
                      :box nil
                      :underline nil
                      :foreground (nord-color "aurora-2")
                      :height 1.3)
  (set-face-attribute 'ekg-tag nil
                      :box nil
                      ;; :background (nord-color "polar-night-0")
                      :foreground (nord-color "aurora-3")
                      :height 1.1)

  (global-set-key (kbd "<f9>") 'ekg-capture)
  (global-set-key (kbd "C-<f9>") 'ii/ekg-consulting-search)
  (global-set-key (kbd "<f10>") 'ekg-show-notes-with-any-tags)
  (global-set-key (kbd "C-<f10>") 'ekg-show-notes-with-all-tags)
  (setq ekg-capture-default-mode 'gfm-mode)
  (add-to-list 'ekg-acceptable-modes 'gfm-mode))

(use-package ekg-extras
  :after ekg
  :bind (("H-e" . ekg-extras/hydra))
  :straight '(ekg-extras :type git
                         :host codeberg
                         :repo "bunnylushington/ekg-extras"))

(defun ii/ekg-add-tag ()
  "Add a tag to an in-process EKG note."
  (interactive)
  (let* ((tags nil)
        (all-tags (ekg-tags))
        (chosen-tags (cl-loop
                      (let* ((candidates (cl-set-difference all-tags tags
                                                            :test #'string=))
                             (input (completing-read
                                     (format-prompt "Tags (%s)" nil
                                                    (string-join tags ", "))
                                     candidates)))
                        (if (string-empty-p input)
                            (cl-return (nreverse tags))
                          (push input tags))))))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^Tags: .+")
      (insert ", ")
      (insert (string-join chosen-tags ", ")))))

(defun ii/ekg-add-title ()
  "Add a title to an in-process EKG note."
  (interactive)
  (let ((title (read-from-minibuffer (format-prompt "Title" nil))))
    (save-excursion
      (goto-char (point-min))
      (insert (format "Title: %s\n" title)))))

(define-key ekg-capture-mode-map (kbd "C-c C-t") #'ii/ekg-add-tag)
(define-key ekg-capture-mode-map (kbd "C-c C-h") #'ii/ekg-add-title)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; consult search

(defun ii/ekg-consulting-search ()
  "Search EKG notes with consult goodness.

Words separated by a space all must be present unless a pipe is the
first character of the search."
  (interactive)
  (require 'consult)
  (let* ((completion (consult--read
                      (consult--dynamic-collection
                          #'ii/ekg-search-completion
                        :min-input 3)
                      :state #'ii/ekg-preview
                      :require-match t
                      :prompt (format-prompt "EKG Text Search" nil)))
         (note (ii/get-note-from-completion completion)))
    (if note
        (ekg-edit note)
      (message "Note not available."))))

(defun ii/get-note-from-completion (completion)
  "Given a completion, return a note object or nil.

The note-id will be an invisible text at the head of the completion
string delimited from the remainder with a colon."
  (let* ((note-id-str (car (s-split ":" (substring-no-properties completion))))
         (note-id (string-to-number note-id-str))
         (note (when (ekg-note-with-id-exists-p note-id)
                 (ekg-get-note-with-id note-id))))
    note))

(defun ii/ekg-preview (action completion)
  "Show a preview note in a consulting preview buffer."
  (pcase action
    ('preview
     (with-current-buffer-window " *ekg completion*" 'action nil
       (erase-buffer)
       (let ((note (ii/get-note-from-completion completion)))
         (when note
           (when (ekg-note-mode note)
             (funcall (ekg-note-mode note)))
           (insert (ekg-note-text note))))))))

(defun ii/ekg-search-completion (input)
  "Prepare the table for the consult--read."
  (let* ((str (substring-no-properties input))
         (res nil))
    (dolist (term (s-split-words str))
      (let ((notes (nconc
                    (ekg-get-notes-with-tag term)
                    (ekg--search-notes term))))
        (setq res
              (cond
               ((null res) notes)
               ((s-prefix? "|" input) (nconc res notes))
               (t (seq-intersection res notes))))))
    (seq-uniq (mapcar #'ii/single-line-note res))))

(defun ii/ekg-first-line (text)
  "Return the first line of text only and truncate at 15 words."
  (let ((lines (s-lines text)))
    (ekg-truncate-at (car lines) 15)))

(defun ii/single-line-note (note &optional template)
  "Prepare the completion table item."
  (format "%s%s: %s"
          (propertize
           (concat (number-to-string (ekg-note-id note)) ":")
           'invisible 't)
          (propertize (s-join ", " (ekg-note-tags note))
                      'face 'ekg-tag)
          (ii/ekg-first-line (ekg-note-text note))))
