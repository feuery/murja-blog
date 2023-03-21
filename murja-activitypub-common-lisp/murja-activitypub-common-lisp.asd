(defsystem "murja-activitypub-common-lisp"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:fset :postmodern :simple-date :simple-date/postgres-glue :easy-routes :cl-json :str)
  :components ((:module "src"
                :components
                ((:file "db")
		 (:file "main")
		 (:file "actor"))))
  :description ""
  :in-order-to ((test-op (test-op "murja-activitypub-common-lisp/tests"))))

(defsystem "murja-activitypub-common-lisp/tests"
  :author ""
  :license ""
  :depends-on ("murja-activitypub-common-lisp"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for murja-activitypub-common-lisp"
  :perform (test-op (op c) (symbol-call :rove :run c)))
