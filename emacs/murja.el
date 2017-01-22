;; -*- lexical-binding: t -*-
;;
;; Depends on https://github.com/tkf/emacs-request

(require 'widget)
(require 'request)
(require 'tabulated-list)

(defvar murja-logged-in-user nil)
(defvar murja-url nil)
(defvar loaded-murja-titles nil)

(defvar murja-error-handler (cl-function (lambda (&rest args &key error-thrown response &allow-other-keys)
				   (setq murja-url nil)
				   (message "Got error: %S - %S" error-thrown))))

(defun murja-title-entry (title)
  (list title
	`[,(assoc-default 'Title title)
	  ,(capitalize (assoc-default 'Month title))
	  ,(prin1-to-string (assoc-default 'Year title))]))

(define-derived-mode murja-title-mode tabulated-list-mode "Murja titles"
  "Major mode for browsing a list of titles in a murja blog instance")	
  
(defun murja-titles ()
  (interactive)
  (when murja-url
    (let ((title-url (concat murja-url "/api/posts/all-titles")))
      (request title-url
	       :parser 'json-read
	       :success (cl-function
			 (lambda (&key data &allow-other-keys)
			   (switch-to-buffer (get-buffer-create (concat "Murja: " murja-url)))
			   (setq loaded-murja-titles data)
			   (murja-title-mode)
			   (setq tabulated-list-format
				 [("Title" 20 t)
				  ("Month" 9 t)
				  ("Year" 4 t)])
			   (setq tabulated-list-entries (mapcar #'murja-title-entry loaded-murja-titles))
			   (message (concat "Tabulated list entries: " (prin1-to-string tabulated-list-entries)))
			   (tabulated-list-print)))
	       :error murja-error-handler))))


(defun murja-main (url username)
  (interactive "sURL to murja instance: \nsUsername: \n")
  (setq murja-url url)
  (let* ((passwd (read-passwd "Password: "))
	 (login-url (concat url "/api/login/login")))
    (setq murja-url url)
    (request login-url
	     :data (json-encode
		    `(("username" . ,username)
		      ("password" . ,passwd)))
	     :headers '(("Content-Type" . "application/json"))
	     :parser 'json-read
	     :success (cl-function
		       (lambda (&key data &allow-other-keys)
			 (setq murja-logged-in-user data)
			 (murja-titles)))
	     :error murja-error-handler)))
