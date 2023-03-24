(defpackage murja-activitypub-common-lisp.actor
  (:use :cl :ql :postmodern)
  (:export :get-person :get-person-from-db :webfinger-query))
(in-package murja-activitypub-common-lisp.actor)

(defun slurp (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun get-person-from-db (account)
  (query "SELECT * FROM blog.Users WHERE username = $1" account :alist))

(defun get-person (id)
  `(("@context" . ("https://www.w3.org/ns/activitystreams"
						     "https://w3id.org/security/v1"))
				      ("id" . "https://feuerx.net/activitypub/feuer")
				      ("type" . "Person")
				      ("preferredUsername" . "Feuer")
				      ("inbox" . "https://feuerx.net/inbox")
				      ("publicKey" . (("id" . "https://feuerx.net/activitypub/feuer#main-key")
						      ("owner" . "https://feuerx.net/activitypub/feuer")
						      ("publicKeyPem" . ,(slurp  #P"/Users/feuer/common-lisp/murja-activitypub-common-lisp/resources/public.pem"))))))

(defun webfinger-query (resource server account)
  (if (string= server "feuerx.net")

      (let ((user 
	      (query "SELECT * FROM blog.Users WHERE username = $1" account :alist)))
	(if user
	    (json:encode-json-alist-to-string `((subject . ,(cdr (assoc :NICKNAME user)))
						(links . (((rel . "self")
							   (type . "application/activity+json")
							   (href . ,(format nil "~a~a~a~a" "https://" server "/activitypub/" account)))))))))))

