;; -*- lexical-binding: t -*-

(require 'tabulated-list)
(require 'cl)

(defvar murja-add-tag '())

(defun murja-cee ()
  (interactive)
  (if murja-add-tag
      (funcall murja-add-tag (read-string "New tag: "))
    (message "murja-add-tag is undefined")))

(defun murja-selected-tag ()
  (elt (cadr (assoc (tabulated-list-get-id) tabulated-list-entries)) 0))

(defvar murja-tags-title ""
  "Keeps track of which blogpost we're editing")

(defun murja-delete-tag ()
  (interactive)
  (let ((tag-to-delete (murja-selected-tag)))
    (with-current-buffer murja-tags-title
      (setq murja-tags (cl-remove-if (lambda (tag)
				       (string= tag tag-to-delete)) murja-tags)))
    (murja-refresh-tags)))
	 
	     
    
  

(define-derived-mode murja-tags-mode tabulated-list-mode "Tags of a murja post"
  "Major mode for managing tags of a murja blog post"
  (make-local-variable 'murja-add-tag)
  (make-local-variable 'murja-tags-title)
  (local-set-key (kbd "c") 'murja-cee)
  (local-set-key (kbd "d") 'murja-delete-tag))

(defun murja-refresh-tags ()
  (let ((local-tags '()))
    (with-current-buffer murja-tags-title
      (setq local-tags murja-tags))
    (setq tabulated-list-format [("Tag" 50 t)])
    (setq tabulated-list-entries (mapcar (lambda (x) (list x `[,x])) local-tags))
    (tabulated-list-print)))

(defun murja-tag-buffer (title)
  (switch-to-buffer (get-buffer-create (concat title ": Tags")))
  (murja-tags-mode)
  (setq murja-tags-title title)
  (setq murja-add-tag (lambda (tag)
			(with-current-buffer title
			  (push tag murja-tags))
			(murja-refresh-tags)))

  (murja-refresh-tags))

(provide 'murja-tags)
