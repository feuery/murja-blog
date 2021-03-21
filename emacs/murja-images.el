;; -*- lexical-binding: t -*-

(defun murja-selected-media-id ()
  (cdr
   (assoc 'id
	  (car
	   (assoc (tabulated-list-get-id) tabulated-list-entries)))))

(defvar *murja-old-buffer* nil)

(defun insert-media-id ()
  (interactive)
  (let ((id (murja-selected-media-id)))
    (switch-to-buffer *murja-old-buffer*)
    (insert "<img src=\"" murja-url "/api/pictures/" id "\" />")))
    

(defvar murja-media-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'insert-media-id)
    map))

(define-derived-mode murja-media-mode tabulated-list-mode "Murja media"
  "Major mode for browsing a list of media in a murja blog instance")

(defun murja-media-entry (media)
  (list media
	`[,(assoc-default 'id media)
	  ,(assoc-default 'name media)]))

(defun murja-list-images ()
  (interactive)
  (let* ((cmd-result (shell-command-to-string (concat murja-script-directory "/murja-client.sh GET " murja-url "/api/pictures/list/all")))
	 (data (murja-json-read cmd-result))
	 (*murja-old-buffer* (current-buffer)))
    (switch-to-buffer (get-buffer-create (concat "Murja images: " murja-url)))
    (murja-media-mode)
    (setq tabulated-list-format [("ID" 10 t)
				 ("Name" 40 t)])
    (setq tabulated-list-entries (mapcar #'murja-media-entry data))
    (tabulated-list-print)))
