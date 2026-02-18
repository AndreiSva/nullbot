(defsystem "nullbot"
  :version "0.0.1"
  :author "Andrei Șova"
  :license "MIT"
  :depends-on (:mapi
               :com.inuoe.jzon
               :dexador
               :bordeaux-threads
               :cl-hash-util
               :quri
               :flexi-streams
               :split-sequence
               :xmls)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A bot for nullring on matrix"
  :in-order-to ((test-op (test-op "nullbot/tests"))))

(defsystem "nullbot/tests"
  :author "Andrei Șova"
  :license "MIT"
  :depends-on (:nullbot
               :rove)
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for nullbot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
