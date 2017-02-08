;; -*- lexical-binding: t -*-

(require 'murja-tags)

(defvar murja-error-handler (cl-function (lambda (&rest args &key error-thrown response &allow-other-keys)
				   (message "Got error: %S - %S" error-thrown))))

(defvar murja-tags '())
(defvar murja-title "")
(defvar murja-id nil)

(defun murja-handle-edit-success (data symbol-status title)
  (let ((id (cdr (assoc 'id data))))
    (if id
	(setq murja-id id))
    (message (concat "Saved. Status is " (prin1-to-string symbol-status)))
    (if (string-equal (symbol-name symbol-status) "success")
	(message (concat title " saved successfully to " murja-url))
      (message (concat "There was a problem saving " title " to " murja-url ". Status is " (prin1-to-string symbol-status))))))

(defun murja-edit-tags ()
  (interactive)
  (murja-tag-buffer murja-title))

(defun murja-save-post-buffer (murja-title)
  (interactive)
  ;; POST /api/posts/post/post
  ;;  {
  ;;   "title": "string",
  ;;   "content": "string",
  ;;   "tags": [
  ;;     "string"
  ;;   ]
  ;; }
  (let ((url (if murja-id
		 (concat murja-url "/api/posts/post/edit")
	       (concat murja-url "/api/posts/post/post")))
	(data (if murja-id
		  (json-encode `((title . ,murja-title)
				 (content . ,(buffer-substring-no-properties (point-min) (point-max)))
				 (tags . ,murja-tags)
				 (id . ,(if (stringp murja-id)
					    (string-to-number murja-id)
					  murja-id))))
		(json-encode `((title . ,murja-title)
			       (content . ,(buffer-substring-no-properties (point-min) (point-max)))
			       (tags . ,murja-tags))))))
    (message (concat "Data: " (prin1-to-string data)))
    (request url
	     :type "POST"
	     :data data
	     :parser 'json-read
	     :headers '(("Content-Type" . "application/json"))
			;; ("Accept" .  "application/json")
	     :success (cl-function
		       (lambda (&key data symbol-status &allow-other-keys)
			 (if data
			     (murja-handle-edit-success data symbol-status murja-title)
			   (message "Lol, data on nil"))))
	     :error murja-error-handler)))

(define-derived-mode murja-post-mode html-mode " murja-post "
  "Mode for writing and editing murja posts"
  (make-local-variable 'murja-tags)
  (make-local-variable 'murja-title)
  (make-local-variable 'murja-id)
  (local-set-key (kbd "C-x C-s") (lambda ()
				   (interactive)
				   (murja-save-post-buffer murja-title)))
  (local-set-key (kbd "C-x C-m t") #'murja-edit-tags))

(defun murja-new-post (title)
  (interactive "sNew title: ")
  (switch-to-buffer (get-buffer-create title))
  (erase-buffer)
  (murja-post-mode)
  (setq murja-title title))




(defun murja-xml-pretty-print (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules.

Copypasted from http://stackoverflow.com/a/570049"
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
  (message "Ah, much better!"))

(defun opening-murja-post-buffer (data)
  (message (prin1-to-string data))
  (save-excursion
    (let ((title (cdr (assoc 'title data)))
	  (id (cdr (assoc 'id data)))
	  (tags (cdr (assoc 'tags data))))
      (switch-to-buffer (get-buffer-create title))
      
      (erase-buffer)
      (insert (cdr
	       (assoc 'content
		      data)))
      (murja-xml-pretty-print (point-min) (point-max))
      (murja-post-mode)
      
      (setq murja-tags (mapcar (lambda (x) x) tags))
      (setq murja-title title)
      (setq murja-id id))))

(defun open-murja-post-buffer (id)
  (if murja-url
      (let ((url (concat murja-url "/api/posts/" (prin1-to-string id))))
	(message (concat "Requesting " url))
	(request url
		 :parser 'json-read
		 :success (cl-function
			   (lambda (&key data symbol-status &allow-other-keys)
			     (if data
				 (opening-murja-post-buffer data)
			       (message (concat "Data is invalid: " (prin1-to-string data) " - "
						(prin1-to-string symbol-status))))))
		 :error murja-error-handler))
    (message "murja-url is nil, did you call murja-main?")))


(provide 'murja-new-post)
