;; -*- mode: lisp -*-

(defsystem vows
  :description "Threading ala promises"
  :VERSION "0.1"
  :author "Ben Hyde"
  :license "http://www.apache.org/licenses/LICENSE-2.0"
  :depends-on ()
  :serial t
  :components ((:file "packages")
               (:file "vows")
               #+5am (:file "tests")))
