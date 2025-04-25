
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
                                    (ellama-generate-commit-message)
                                  ;; Catch potential errors during generation
                                  (error (message "Ellama commit message generation failed: %s" err)
                                         nil))))) ; Return nil on error
      ;; If generation was requested, successful, and returned text:
      (when (and generate? generated-message (not (string-empty-p generated-message)))
        (insert generated-message)
        (goto-char (point-min)) ; Move cursor to beginning
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
