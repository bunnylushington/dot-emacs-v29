(transient-define-prefix ii/bookmark-menu/show ()
  "Show/Hide commands"
  ["Show/Hide Bookmarks\n"
   ["Global"
    ("." "All" bmkp-bmenu-show-all)
    (">" "Only Marked" bmkp-bmenu-toggle-show-only-marked)
    ("<" "Only Unmarked" bmkp-bmenu-toggle-show-only-unmarked)]

   ["System"
    ("#" "Autonamed" bmkp-bmenu-show-only-autonamed-bookmarks)
    ("A" "Autofile" bmkp-bmenu-show-only-autofile-bookmarks)
    ("X" "Temporary" bmkp-bmenu-show-only-temporary-bookmarks)]

   ["By Pattern"
    :pad-keys 1
    ("P A" "Annotation match" bmkp-bmenu-filter-annotation-incrementally :transient leave)
    ("P B" "Name match" bmkp-bmenu-filter-file-name-incrementally :transient leave)
    ("P F" "File match" bmkp-bmenu-filter-file-name-incrementally :transient leave)
    ("P T" "Tag match" bmkp-bmenu-filter-tags-incrementally :transient leave)]]

  ["By Type"
   [""
    ("B" "Non-file" bmkp-bmenu-show-only-non-file-bookmarks)
    ("F" "File and directory" bmkp-bmenu-show-only-file-bookmarks)
    ("G" "Gnus" bmkp-bmenu-show-only-gnus-bookmarks)
    ("I" "Info" bmkp-bmenu-show-only-info-bookmarks)
    ("K" "Desktop" bmkp-bmenu-show-only-desktop-bookmarks)]

   [""
    ("M" "Man page" bmkp-bmenu-show-only-man-bookmarks)
    ("M-d" "Dired" bmkp-bmenu-show-only-dired-bookmarks)
    ("M-i" "Image file" bmkp-bmenu-show-only-image-bookmarks)
    ("M-u" "URL" bmkp-bmenu-show-only-url-bookmarks)
    ("O" "Orphaned local file" bmkp-bmenu-show-only-orphaned-local-file-bookmarks)]

   [""
    ("Q" "Function" bmkp-bmenu-show-only-function-bookmarks)
    ("R" "Region" bmkp-bmenu-show-only-region-bookmarks)
    ("T" "Tagged" bmkp-bmenu-show-only-tagged-bookmarks)
    ("U" "Untagged" bmkp-bmenu-show-only-untagged-bookmarks)
    ("V" "Variable-list" bmkp-bmenu-show-only-variable-list-bookmarks)]])

(transient-define-prefix ii/bookmark-menu/sort ()
  "Sorting commands"
  ["Bookmark Sorting\n"
   ["Update Sort"
    :pad-keys 1
    ("r" "Reverse sort direction" bmkp-reverse-sort-order :transient t)
    ("C-r" "Compliment sort predicate" bmkp-reverse-multi-sort-order :transient t)
    ("s" "Cycle sort direction" bmkp-bmenu-change-sort-order-repeat :transient t)]

   ["Start sort with..."
    (">" "Marked" bmkp-bmenu-sort-marked-before-unmarked)
    ("D" "Flagged" bmkp-bmenu-sort-flagged-before-unflagged)
    ("*" "Modified" bmkp-bmenu-sort-modified-before-unmodified)
    ("t" "Tagged" bmkp-bmenu-sort-tagged-before-untagged)
    ("a" "Annotated" bmkp-bmenu-sort-annotated-before-unannotated)]

   ["Bookmark Criteria"
    ("0" "Creation Time" bmkp-bmenu-sort-by-creation-time)
    ("b" "Last Buffer/File Access" bmkp-bmenu-sort-by-last-buffer-or-file-access)
    ("d" "Last Bookmark Access" bmkp-bmenu-sort-by-last-bookmark-access)
    ("g" "Gnus Thread" bmkp-bmenu-sort-by-Gnus-thread)
    ("i" "Info Node" bmkp-bmenu-sort-by-Info-node-name)
    ("I" "Info Position" bmkp-bmenu-sort-by-Info-position)
    ("k" "Bookmark Type" bmkp-bmenu-sort-by-bookmark-type)
    ("n" "Bookmark Name" bmkp-bmenu-sort-by-bookmark-name)
    ("u" "URL" bmkp-bmenu-sort-by-url)
    ("v" "Bookmark Visit Frequency" bmkp-bmenu-sort-by-bookmark-visit-frequency)]

   ["File Criteria"
    :pad-keys 1
    ("f d" "Last local file access" bmkp-bmenu-sort-by-last-local-file-access)
    ("f k" "Local file type" bmkp-bmenu-sort-by-local-file-type)
    ("f n" "File name" bmkp-bmenu-sort-by-file-name)
    ("f s" "Local file size" bmkp-bmenu-sort-by-local-file-size)
    ("f u" "Last local file update" bmkp-bmenu-sort-by-last-local-file-update)]])

(transient-define-prefix ii/bookmark-menu/tags ()
  "Tag commands"
  ["Bookmark Tags\n"
   ["Edit"
    ("+" "Add tags" bmkp-add-tags)
    ("e" "Edit tags" bmkp-bmenu-edit-tags)
    ("l" "List tags" bmkp-list-all-tags)
    ("v" "Set tag value" bmkp-bmenu-set-tag-value)
    ("-" "Remove tags" bmkp-remove-tags)
    ("0" "Remove all tags" bmkp-remove-all-tags)
    ("d" "Remove tags from all" bmkp-remove-tags-from-all)
    ("r" "Rename tag" bmkp-rename-tag)]

   ["Copy/Paste"
    :pad-keys 1
    ("c" "Copy from bookmark" bmkp-bmenu-copy-tags)
    ("M-w" "Copy tags" bmkp-bmenu-copy-tags)
    ("C-y" "Paste tags to bookmark, add" bmkp-bmenu-paste-add-tags)
    ("q" "Paste tags to bookmark, replace" bmkp-bmenu-paste-replace-tags)]

   ["Marks and Visibility"
    ("S" "Show only tagged bookmarks" bmkp-bmenu-show-only-tagged-bookmarks)
    ("U" "Show only untagged bookmarks" bmkp-bmenu-show-only-untagged-bookmarks)]]

  [["Edit (Marked Bookmarks)"
    :pad-keys 1
    ("> +" "Add tags" bmkp-bmenu-add-tags-to-marked)
    ("> -" "Remove tags" bmkp-bmenu-remove-tags-from-marked)
    ("> e" "Edit" bmkp-bmenu-edit-marked)
    ("> l" "List" bmkp-bmenu-list-tags-of-marked)
    ("> v" "Set Value" bmkp-bmenu-set-tag-value-for-marked)]

   ["Paste (Marked Bookmarks)"
    :pad-keys 1
    ("> C-y" "Paste tags, add" bmkp-bmenu-paste-add-tags-to-marked)
    ("> p" "Paste tags, add" bmkp-bmenu-paste-add-tags-to-marked)
    ("> q" "Paste tags, replace" bmkp-bmenu-paste-replace-tags-for-marked)]

   ["Unmark by Tag"
    :pad-keys 1
    ("u %" "Regexp" bmkp-bmenu-unmark-bookmarks-tagged-regexp)
    ("u *" "All" bmkp-bmenu-unmark-bookmarks-tagged-all)
    ("u +" "Some" bmkp-bmenu-unmark-bookmarks-tagged-some)]

   ["Mark by Tag"
    :pad-keys 1
    ("m %" "Regexp" bmkp-bmenu-mark-bookmarks-tagged-regexp)
    ("m *" "All" bmkp-bmenu-mark-bookmarks-tagged-all)
    ("m +" "Some" bmkp-bmenu-mark-bookmarks-tagged-some)]])

(transient-define-prefix ii/bookmark-menu/anywhere ()
  [["Navigation"
    :pad-keys 1
    ("," "Show bookmarks for this buffer" bmkp-this-file/buffer-bmenu-list)
    ("n" "Next in buffer" bmkp-next-bookmark-this-file/buffer :transient t)
    ("p" "Previous in buffer" bmkp-previous-bookmark-this-file/buffer :transient t)
    ("C-c" "Open Bookmark List" bookmark-bmenu-list)]

   ["Set..."
    :pad-keys 1
    ("RET" "Toggle auto bookmark here" bmkp-toggle-autonamed-bookmark-set/delete)
    ("m" "Here" bmkp-bookmark-set-confirm-overwrite)
    ("K" "Current desktop" bmkp-set-desktop-bookmark)
    ("y" "Bookmark file" bmkp-set-bookmark-file-bookmark)
    ("c F" "Function bookmark" bmkp-make-function-bookmark)
    ("c f" "File" bmkp-file-target-set)
    ("c a" "Autoname file" bmkp-autofile-set)
    ("c u" "URL" bmkp-url-target-set)
    ("M-w" "Snippet" bmkp-set-snippet-bookmark)]

   ["Navlist"
    (":" "Set navlist to type" bmkp-choose-navlist-of-type)
    ("B" "Set navlist to bookmark-list bookmark" bmkp-choose-navlist-from-bookmark-list)
    ("N" "Open bookmark-list for bookmarks in navlist" bmkp-navlist-bmenu-list)
    ("f" "Next bookmark in navlist" bmkp-next-bookmark :transient t)
    ("b" "Previous bookmark in navlist" bmkp-previous-bookmark :transient t)]

   ["Bookmarks File"
    :pad-keys 1
    ("0" "Create Empty Bookmark File" bmkp-empty-file)
    ("x" "Toggle Making Bookmarks Temporary" bmkp-toggle-autotemp-on-set)
    ("C-k" "Delete bookmark at point" bmkp-delete-bookmarks)
    ("C-l" "Switch to bookmark file for current file/buffer"
     bmkp-switch-to-bookmark-file-this-file/buffer)
    ("C-s" "Save bookmarks for current file/buffer" bmkp-save-bookmarks-this-file/buffer)]]

  [["Highlighting"
    :pad-keys 1
    ("h" "Highlight bookmark in current buffer" bmkp-light-bookmark-this-buffer)
    ("u" "Unhighlight bookmark in current buffer" bmkp-unlight-bookmark-this-buffer)
    ("=" "List bookmarks highlighted at point" bmkp-bookmarks-lighted-at-point)
    ("C-u" "Unlighlight bookmark at point" bmkp-unlight-bookmark-on-this-line)]])

;; Note that these override the xisting prefix keys in the bmenu-mode-map
(keymap-set bookmark-bmenu-mode-map "s" #'ii/bookmark-menu/sort)
(keymap-set bookmark-bmenu-mode-map "S" #'ii/bookmark-menu/show)
(keymap-set bookmark-bmenu-mode-map "T" #'ii/bookmark-menu/tags)
(keymap-set global-map "C-c C-c" #'ii/bookmark-menu/anywhere)
