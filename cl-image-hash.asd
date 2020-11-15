(defsystem "cl-image-hash"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-image-hash/tests"))))

(defsystem "cl-image-hash/tests"
  :author ""
  :license ""
  :depends-on ("cl-image-hash"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-image-hash"
  :perform (test-op (op c) (symbol-call :rove :run c)))
