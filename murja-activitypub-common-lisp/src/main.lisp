(defpackage murja-activitypub-common-lisp
  (:use :cl :ql :easy-routes :postmodern)
  (:import-from :fset :wb-map))
(in-package :murja-activitypub-common-lisp)

(defparameter *server* (hunchentoot:start (make-instance 'easy-routes:routes-acceptor :port 3001)))

;;(hunchentoot:stop *server*)

(defun @json (next)
  (setf (hunchentoot:content-type*) "application/json")
  (funcall next))

(defun @transaction (next)
  (format t "Moro from transaction~%")
  (with-connection '("blogdb" "blogadmin" "blog" "localhost" :pooled-p t)
    (with-transaction ()
      (funcall next))))

(defroute posts ("/posts"
		 :method :get
		 :decorators (@transaction @json))
    ()
  (format nil "~a"
	  (query "SELECT * FROM blog.post" :json-strs)))



(format t "~%murja-activitypub-common-lisp loaded~%")
