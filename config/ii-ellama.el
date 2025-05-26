(keymap-set ellama-session-mode-map "M-*"
            'ellama-chat-send-last-message)

(setq ellama-provider
      (make-llm-gemini
       :key (auth-source-pick-first-password
             :host '("generativelanguage.googleapis.com"))
       :chat-model "gemini-2.5-pro-preview-03-25"))
(setq
 ellama-response-format "markdown"
 ellama-major-mode 'markdown-mode
 ellama-assistant-nick "Robot Friend"
 ellama-user-nick "Bunny"
 ellama-fill-paragraphs nil)
(add-hook 'ellama-chat-mode-hook #'visual-line-mode)

(defun ii/magit-offer-ellama-commit-message ()
  "Offer to generate commit message with Ellama when initiating a Magit commit.
This function is intended for `git-commit-setup-hook'."
  ;; Check if the function exists, otherwise do nothing silently.
  (when (fboundp 'ellama-generate-commit-message)
    (let* ((message-prompt "Generate commit message with Ellama?")
           (generate? (yes-or-no-p message-prompt))
           (generated-message (when generate?
                                (condition-case err
                                    ;; Call the actual generation function
                                    (progn
                                      (insert "# Generating message...")
                                      (newline)
                                      (ellama-generate-commit-message))
                                  ;; Catch potential errors during generation
                                  (error (message "Ellama commit message generation failed: %s" err)
                                         nil))))) ; Return nil on error
      ;; If generation was requested, successful, and returned text:
      (when (and generate? generated-message (not (string-empty-p generated-message)))
        (insert generated-message)
        (goto-char (point-min))
        (message "Ellama generated commit message inserted.")))))

(add-hook 'git-commit-setup-hook #'ii/magit-offer-ellama-commit-message 90)



(setq ellama-generate-commit-message-template "<INSTRUCTIONS>
You are professional software developer.

Write concise commit message based on diff in the following format:

<FORMAT>

First line should contain one of the words 'build', 'ci',
'docs', 'feat', 'fix', 'perf', 'refactor', 'test' or 'chore' indicating
what kind of change this is and short title described major change in
functionality.  Then one empty line. Then detailed description of all
changes.

</FORMAT>

<EXAMPLE>
feat: Improve abc

Improved abc feature by adding new xyz module.
</EXAMPLE>

**Reply with commit message only without any quotes.**
</INSTRUCTIONS>

<DIFF>
%s
</DIFF>
")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; release notes/squash messages

(setq ii/ellama-generate-squash-commit-message-template
      "You are a helpful assistant tasked with generating release notes.

I will provide you with the output of the command '%s'.

Based on the following git commit log, please generate release notes for
end users.  These notes should not contain sensitive information or
implementation details.  The relee notes will also be used as a git
merge message.

The notes should have three sections:

  1. A single title line describing the changes.  The title will
  comprise one of the words 'build', 'ci', 'docs', 'feat', 'fix',
  'perf', 'refactor', 'test', or 'chore' followed by a colon and then
  the title itself.

  2. A prose summary indicating the theme and broad overview of the
  changes.

  3. A bulleted list of specific changes.  Each bullet point should
  concisely describe a commit.  Try to make these human-readable and
  focus on the impact or feature rather than just repeating the raw
  commit message if it's too terse or technical.  Please leave an empty
  line between bullets.

Here is the git log output:
```
%s
```
Please provide the release notes and only the release notes now.")

(defun ii/ellama-generate-squash-message (rev &optional start)
  "Generate release notes from the relevant log messages.

REV is the revised branch, specifically the parameter required by
`magit-merge-squash' and START is the branch being merged into (usually
main or staging)"
  (when (y-or-n-p "Generate a CHANGELOG entry?")
    (require 's)
    (let* ((git-branch-command "git branch --show-current")
           (current-branch
            (or start
                (s-chomp (shell-command-to-string git-branch-command))))
           (git-log-format "--pretty=format:'%h %s (%an, %ar)'%n%b")
           (git-log-command (format "git log %s %s..%s"
                                    git-log-format current-branch rev))
           (git-log-output (shell-command-to-string git-log-command))
           (prompt (format ii/ellama-generate-squash-commit-message-template
                           git-log-command git-log-output))
           (output-buffer (get-buffer-create "*ii/magit squash message*")))
      (message "Generating commit message with ellama.")
      (ellama-stream prompt
                     :buffer output-buffer
                     :ephemeral-session t
                     :on-done (lambda (res)
                                (kill-new res)
                                (ii/prepend-to-current-changelog res)
                                (message "Squash commit message copied to kill ring."))))))

(advice-add 'magit-merge-squash
            :after #'ii/ellama-generate-squash-message)

(defun ii/prepend-to-current-changelog (text)
  (let* ((ts-format "%a %b %d, %Y -- %H:%M:%S %Z")
         (ts (format-time-string ts-format (current-time)))
         (file (concat (project-root (project-current)) "CHANGELOG"))
         (buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (goto-char (point-min))
      (insert (format "*** %s\n" ts))
      (insert (format "*** %s <%s>\n\n"
                      (magit-get "user.name")
                      (magit-get "user.email")))
      (insert (concat (ii/fill-string-paragraphs text 76)) "\n\n")
      (save-buffer)
      (kill-buffer buffer))))

(defun ii/fill-string-paragraphs (input-string &optional fill-width)
    "Fill paragraphs in INPUT-STRING.
Optional FILL-WIDTH specifies the fill column.
If FILL-WIDTH is nil, uses the current buffer's `fill-column`
or the default if called in a context without a current buffer's value.
Returns the string with paragraphs filled."
  (with-temp-buffer
    (insert input-string)
    (let ((fill-column (or fill-width fill-column)))
      (goto-char (point-min))
      (while (not (eobp))
        (fill-paragraph nil)
        (unless (eobp) (forward-line 1))))
    (buffer-string)))
