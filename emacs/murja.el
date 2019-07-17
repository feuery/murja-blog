;; -*- lexical-binding: t -*-

;; depends on https://github.com/rejeep/f.el

(require 'widget)
(require 'tabulated-list)
(require 'murja-new-post)
(require 'json)
(require 'f)

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

(defun murja-json-read (str)
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (json-read)))

(defvar script-directory (file-name-directory load-file-name))

(defun murja-titles ()
  (interactive)
  (message (concat "murja-titles called with url " murja-url))
  (when murja-url
    (let* ((title-url (concat murja-url "/api/posts/all-titles"))
	   (cmd-result (shell-command-to-string
			(concat script-directory "/murja-client.sh GET " title-url "")))
	   (data (murja-json-read cmd-result)))
      (switch-to-buffer (get-buffer-create (concat "Murja: " murja-url)))
      (setq loaded-murja-titles data)
      (murja-title-mode)
      (setq tabulated-list-format
	    [("Title" 40 t)
	     ("Month" 9 t)
	     ("Year" 4 t)])
      (setq tabulated-list-entries (mapcar #'murja-title-entry loaded-murja-titles))
      (tabulated-list-print))))


(defun murja-main (url username)
  (interactive "sURL to murja instance: \nsUsername: \n")
  (let* ((passwd (read-passwd "Password: "))
	 (login-url (concat url "/api/login/login"))
	 (json (json-encode
		`(("username" . ,username)
		  ("password" . ,passwd)))))
    (f-write-text json 'utf-8 (concat script-directory "/input.json"))
    (let ((cmd-result (shell-command-to-string
		       (concat script-directory "/murja-client.sh POST " login-url " " skript-directory "/input.json" ))))
      ;; (message "CMD-result %s" cmd-result)
      (setq murja-url url)
      (f-delete (concat script-directory "/input.json"))
      (let ((data (murja-json-read cmd-result)))
	(message "Logged in!")
	(setq murja-logged-in-user data)
	(murja-titles)))))

(provide 'murja)
