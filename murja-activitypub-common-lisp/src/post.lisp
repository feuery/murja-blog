(defpackage murja-activitypub-common-lisp.post
  (:import-from :murja-activitypub-common-lisp.actor :get-person-from-db)
  (:export :load-posts-as-activitypub)
  (:use :cl :ql :postmodern))

(in-package murja-activitypub-common-lisp.post)

(defun format-timestamp (ts)
  (multiple-value-bind (year month day hour min sec)
      (simple-date:decode-timestamp ts)
    (format nil "~d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" year month day hour min sec)))

(defun post-alist->activitypub (post)
  ;; get-personin ehkä pitäis palauttaa alist eikä json-string
  (let* ((person-apub (get-person (cdr (assoc :CREATOR-ID post))))
	 (post-id (format nil "https://feuerx.net/blog/post/~a" (cdr (assoc :ID post))))
	 (actor (cdr (assoc "id" person-apub :test #'string=))))
    `((@context . "https://www.w3.org/ns/activitystreams")
      (id . ,post-id)
      (type . "Create")
      (actor . ,actor)
      (object . ((id . ,post-id)
		 (type . "Note")
		 (published . ,(format-timestamp (cdr (assoc :CREATED-AT post))))
		 (attributedTo . ,actor)
		 (inReplyTo . nil)
		 (content . ,(cdr (assoc :CONTENT post)))
		 (to . "https://www.w3.org/ns/activitystreams#Public"))))))
						 

(defun load-posts-as-activitypub (&optional (howmany 10))
  (json:encode-json-to-string
   (mapcar #'post-alist->activitypub
	   (query "SELECT * FROM blog.Post LIMIT $1" howmany :alists))))
