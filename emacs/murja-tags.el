(require 'tabulated-list)

(define-derived-mode murja-tags-mode tabulated-list-mode "Tags of a murja post"
  "Major mode for managing tags of a murja blog post")

  (list title
	`[,(assoc-default 'Title title)
	  ,(capitalize (assoc-default 'Month title))
	  ,(prin1-to-string (assoc-default 'Year title))])

(defun murja-tag-buffer (title tags)
  (switch-to-buffer (get-buffer-create (concat title ": Tags")))
  (murja-tags-mode)
  (setq tabulated-list-format [("Tag" 50 t)])
  (setq tabulated-list-entries (mapcar (lambda (x) (list x `[,x])) tags))
  (tabulated-list-print))

(provide 'murja-tags)
