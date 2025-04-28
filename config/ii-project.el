(defun ii/open-vterm-in-directory (path buffer-name)
  "Open Vterm in the directory containing PATH."
  (unless (require 'vterm nil t)
    (error "Vterm package is not available"))
  (let* ((target-path (expand-file-name path)) ; Ensure absolute path
         (dir (if (file-directory-p target-path)
                  target-path
                (file-name-directory target-path))))
    (unless dir (error "Could not determine directory from %s" path))
    (let ((default-directory dir)
          (current-prefix-arg buffer-name))
      (vterm)
      (message "Opened vterm in %s" dir))))

(defun ii/project-vterm ()
  "Open a VTerm in the project root"
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm")))
    (ii/open-vterm-in-directory default-directory vterm-buffer-name)))

(defun ii/project-magit-status ()
  "Open the magit-status window in the project root."
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (magit-status default-directory)))

(defun ii/project-three-pane ()
  (interactive)
  (let* ((default-directory (project-root (project-current t))))
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (windmove-down)
    (ii/project-vterm)
    (windmove-right)
    (project-dired)
    (ii/project-magit-status)
    (select-window (car (window-list)))
    (scratch-buffer)))

(custom-set-variables
 '(project-switch-commands
   '((ii/project-vterm "VTerm" "v")
     (project-dired "Dired")
     (project-find-file "Find File")
     (ii/project-three-pane "Three Pane" "t")
     (ii/project-magit-status "Magit Status" "m"))))

(defun ii/project-current-short-name ()
  "A short name for the current project or nil."
  (interactive)
  (let ((current (project-current)))
    (if current
        (let* ((name (caddr current))
               (prj (cadr (reverse (file-name-split name)))))
          prj)
      nil)))
