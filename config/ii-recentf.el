(customize-set-value 'recentf-max-saved-items 50)

(defun ii/recentf-project-filter (l)
  "Filter the list of menu-elements L to show relative filenames.
Filenames are relative to the `default-directory'."
  (remove nil (mapcar (lambda (menu-element)
            (let* ((ful (recentf-menu-element-value menu-element))
                   (prj (project-root (project-current))))
              (message "prj %s -- ful %s" prj ful)
              (when (string-prefix-p prj ful)
                (message "MATCH prj %s -- ful %s" prj ful)
                menu-element))) l)))

; (customize-set-value 'recentf-menu-filter #'ii/recentf-project-filter)
