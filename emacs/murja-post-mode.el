;; -*- lexical-binding: t -*-
;;
;; Depends on https://github.com/tkf/emacs-request

(require 'widget)
(require 'request)

(defvar murja-error-handler (cl-function (lambda (&rest args &key error-thrown response &allow-other-keys)
				   (message "Got error: %S - %S" error-thrown))))

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

(defun murja-handle-edit-success (symbol-status title)
  (message (concat "Saved. Status is " (prin1-to-string symbol-status)))
  (if (string-equal (symbol-name symbol-status) "success")
      (message (concat title " saved successfully to " murja-url))
    (message (concat "There was a problem saving " title " to " murja-url ". Status is " (prin1-to-string symbol-status)))))
    

(defun murja-save-post-buffer (murja-title murja-id murja-tags)
  (interactive)
  ;; POST /api/posts/post/post
  ;;  {
  ;;   "title": "string",
  ;;   "content": "string",
  ;;   "tags": [
  ;;     "string"
  ;;   ]
  ;; }
  (let ((url (concat murja-url "/api/posts/post/edit"))
	(data (json-encode `((title . ,murja-title)
			     (content . ,(buffer-substring-no-properties (point-min) (point-max)))
			     (tags . ,murja-tags)
			     (id . ,(string-to-number murja-id))))))
    (request url
	     :type "POST"
	     :data data
	     ;; :parser 'json-read
	     :headers '(("Content-Type" . "application/json"))
	     :success (cl-function
		       (lambda (&key data symbol-status &allow-other-keys)
			 (murja-handle-edit-success symbol-status murja-title)))
	     :error murja-error-handler)))
					

(defun opening-murja-post-buffer (data)
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
      (html-mode)
      (local-set-key (kbd "C-x C-s") (lambda ()
					(interactive)
					(murja-save-post-buffer title (prin1-to-string id) tags))))))

(defun open-murja-post-buffer (id)
  (if murja-url
      (let ((url (concat murja-url "/api/posts/" (prin1-to-string id))))
	(message (concat "Requesting " url))
	(request url
		 :parser 'json-read
		 :success (cl-function
			   (lambda (&key data &allow-other-keys)
			     (opening-murja-post-buffer data)))
		 :error murja-error-handler))
    (message "murja-url is nil, did you call murja-main?")))

(provide 'murja-post-mode)
