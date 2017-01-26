;; -*- lexical-binding: t -*-
;;
;; Depends on https://github.com/tkf/emacs-request

(require 'widget)
(require 'request)

(defvar murja-error-handler (cl-function (lambda (&rest args &key error-thrown response &allow-other-keys)
				   (setq murja-url nil)
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

(defun open-murja-post-buffer (id)
  (if murja-url
      (let ((url (concat murja-url "/api/posts/" (prin1-to-string id))))
	(message (concat "Requesting " url))
	(request url
		 :parser 'json-read
		 :success (cl-function
			   (lambda (&key data &allow-other-keys)
			     (save-excursion

			       (let ((title (cdr (assoc 'title data))))
				 (switch-to-buffer (get-buffer-create title))
				 (erase-buffer)			       

				 (insert (cdr
					  (assoc 'content
						 data)))

				 (murja-xml-pretty-print (point-min) (point-max))
				 (html-mode)))))
		 :error murja-error-handler))
    (message "murja-url is nil, did you call murja-main?")))

(provide 'murja-post-mode)
