;; -*- lexical-binding: t -*-
;;
;; Depends on https://github.com/tkf/emacs-request

(require 'widget)
(require 'request)

(require 'murja-tags)

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

(defvar murja-tags '())
(defvar murja-title "")
    

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

(defun murja-edit-tags ()
  (interactive)
  (murja-tag-buffer murja-title murja-tags))

(define-minor-mode murja-post-edit
  "Defines a few keybindings for communicating with a murja instance"
  :lighter " murja "
  :keymap (let ((map (make-sparse-keymap)))
	    ;; TODO: Come up with a less stupid system for keychords
	    (define-key map (kbd "C-x C-m t") #'murja-edit-tags)
	    map)
  (make-local-variable 'murja-tags)
  (make-local-variable 'murja-title))
	    
	    
					

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
      (html-mode)
      (murja-post-edit)
      (local-set-key (kbd "C-x C-s") (lambda ()
				       (interactive)
				       (murja-save-post-buffer title (prin1-to-string id) tags)))
      (setq murja-tags tags)
      (setq murja-title title))))

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

(provide 'murja-post-mode)
