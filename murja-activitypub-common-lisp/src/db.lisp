(defpackage murja-activitypub-common-lisp.db
  (:use :cl :ql :postmodern :simple-date :simple-date-cl-postgres-glue)
  (:import-from :fset :wb-map))

(in-package :murja-activitypub-common-lisp.db)

(setf cl-postgres:*sql-readtable*
        (cl-postgres:copy-sql-readtable
         simple-date-cl-postgres-glue:*simple-date-sql-readtable*))



(format t "db package loaded ~%")

;; (with-connection '("blogdb" "blogadmin" "blog" "localhost" :pooled-p t)
;;   (query "select title, tags, created_at from blog.post order by created_at desc" :row))
