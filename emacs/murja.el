;; -*- lexical-binding: t -*-
;;
;; Depends on https://github.com/tkf/emacs-request

(require 'widget)
(require 'request)
(require 'tabulated-list)
(require 'murja-new-post)
(require 'json)

(defvar murja-logged-in-user nil)
(defvar murja-url nil)
(defvar loaded-murja-titles nil)

(defun murja-title-entry (title)
  (list title
	`[,(assoc-default 'Title title)
	  ,(capitalize (assoc-default 'Month title))
	  ,(prin1-to-string (assoc-default 'Year title))]))

(defvar murja-title-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'open-murja-post)
    (define-key map (kbd "c") #'murja-new-post)
    map))

(define-derived-mode murja-title-mode tabulated-list-mode "Murja titles"
  "Major mode for browsing a list of titles in a murja blog instance")

(defun murja-selected-post-id ()
  (cdr
   (assoc 'Id
	  (car
	   (assoc (tabulated-list-get-id) tabulated-list-entries)))))

(defun open-murja-post ()
  (interactive)
  (message "Opening murja-post-buffer!")
  (open-murja-post-buffer (murja-selected-post-id)))

  
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
				 [("Title" 40 t)
				  ("Month" 9 t)
				  ("Year" 4 t)])
			   (setq tabulated-list-entries (mapcar #'murja-title-entry loaded-murja-titles))
			   (tabulated-list-print)))
	       :error murja-error-handler))))


(defun murja-main (url username)
  (interactive "sURL to murja instance: \nsUsername: \n")
  (let* ((passwd (read-passwd "Password: "))
	 (login-url (concat url "/api/login/login")))
    (setq murja-url url)
    (message (concat "Trying to log in to " login-url))
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
	     :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                (message "Got error: %S" error-thrown))))))

(provide 'murja)
