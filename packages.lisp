;; -*- mode: lisp -*-

(in-package "COMMON-LISP-USER")

(defpackage "VOWS"
  (:use "COMMON-LISP" #+5am "FIVEAM")
  (:export "VOW" "PROMISE-KEEPER" "*PROMISE-KEEPER*" "FULFILLED?" "FULFIL" "FIND-FUFILLED"
           "BREAK-VOW" "BROKEN-VOW"))
