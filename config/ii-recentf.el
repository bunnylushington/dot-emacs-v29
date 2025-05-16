(customize-set-value 'recentf-max-saved-items 500)

(defun ii/project-recentf-open ()
  "Like `recentf-open' but limited to current project"
  (interactive)
  (if (project-current)
      (let ((file (completing-read
                   (format-prompt "Open recent project file" nil)
                   (ii/recentf-filter-current-project) nil t)))
        (when file
          (funcall recentf-menu-action file)))
    (message "No current project found")))

(defun ii/project-recentf-open-files (&optional dir)
  "Like `recentf-open-files' but limited to current project"
  (interactive)
  (let ((target-directory (or dir default-directory))
        (proj (project-current nil (or dir default-directory))))
    (if proj
        (recentf-open-files
         (ii/recentf-current-project-files proj))
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
