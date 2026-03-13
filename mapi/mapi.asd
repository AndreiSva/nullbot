(defsystem "mapi"
  :version "0.0.1"
  :author "Andrei Șova"
  :license "MIT"
  :depends-on (:com.inuoe.jzon
               :dexador
               :bordeaux-threads
               :cl-hash-util
               :quri
               :split-sequence
               :flexi-streams)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "core" :depends-on ("package"))
                 (:file "commands" :depends-on ("package" "core"))
                 (:file "util" :depends-on ("package" "core")))))
  :description "Library for interacting with the Matrix Client-Server API"
  :in-order-to ((test-op (test-op "mapi/tests"))))

(defsystem "mapi/tests"
  :author "Andrei Șova"
  :license "MIT"
  :depends-on (:mapi
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mapi"
  :perform (test-op (op c) (symbol-call :rove :run c)))
