(defpackage murja-activitypub-common-lisp
  (:use :cl :ql :easy-routes :postmodern
	:murja-activitypub-common-lisp.actor
	:murja-activitypub-common-lisp.post)
  (:import-from :fset :wb-map))
(in-package :murja-activitypub-common-lisp)

(defparameter *server* (hunchentoot:start (make-instance 'easy-routes:routes-acceptor :port 3001)))

;;(hunchentoot:stop *server*)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  (with-connection '("blogdb" "blogadmin" "blog" "localhost" :pooled-p t)
    (with-transaction ()
      (funcall next))))

(defroute actor ("/activitypub/feuer"
		 :method :get
		 :decorators (@transaction @json))
    ()
  (json:encode-json-alist-to-string (get-person "feuer")))

(defroute webfinger ("/.well-known/webfinger"
		     :method :get
		     :decorators (@transaction @json))
    (&get resource)
  ;; example resource
  ;; acct:bob@my-example.com

  (let* ((account-with-server (second (str:split ":" resource)))
	 (acc-split (str:split "@" account-with-server))
	 (account (first acc-split))
	 (server (second acc-split)))
    (webfinger-query resource server account)))


(defroute activitypub-posts
    ("/activitypub/posts"
     :method :get
     :decorators (@transaction @json))
    ()
    (load-posts-as-activitypub))
    
  



(format t "~%murja-activitypub-common-lisp loaded~%")
