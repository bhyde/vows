;; -*- mode: lisp -*-

(in-package "COMMON-LISP-USER")

(defpackage "VOWS"
  (:use "COMMON-LISP")
  (:export "VOW" "PROMISE-KEEPER" "*PROMISE-KEEPER*" "FULFILLED?" "FULFIL" "FIND-FUFILLED"))
