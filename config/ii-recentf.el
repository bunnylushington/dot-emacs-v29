(recentf-mode 1)
(customize-set-value 'recentf-max-saved-items 500)

(global-set-key (kbd "C-c r") #'ii/project-recentf-open)

(defun ii/project-recentf-open ()
  "Like `recentf-open' but try current project files first."
  (interactive)
  (let ((file (completing-read
               (format-prompt "Open recent file" nil)
               (ii/recentf-current-project-files) nil t)))
    (when file
      (funcall recentf-menu-action file))))

(defun ii/project-recentf-open-files (&optional dir)
  "Like `recentf-open-files' but limited to current project"
  (interactive)
  (let ((target-directory (or dir default-directory))
        (proj (project-current nil (or dir default-directory))))
    (if proj
        (let ((files (ii/recentf-current-project-files proj)))
          (if files
              (recentf-open-files files)
            (message "No recent project files found.")))
      (message "No current project found."))))

(defun ii/recentf-current-project-files (&optional project)
  "Filter recentf-list to include files from the current project."
  (let* ((proj (or project (project-current)))
         (root (when proj (project-root proj))))
            (if root
                (let ((canonical-root (expand-file-name
                                       (file-name-as-directory root))))
                  (seq-filter
                   (lambda (file)
                     (file-in-directory-p
                      (expand-file-name file) canonical-root))
                   recentf-list))
              recentf-list)))
