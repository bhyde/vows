;; -*- mode: lisp -*-

(in-package "VOWS")

;; ekk!



(defun try-it ()
  (loop
     with z = (loop for i below 10
                 collect (vow #'(lambda () (let ((n (random 10))) (sleep n) (values :hi n)))))
     while z
     as v = (find-fulfiled z)
     do
       (setf z (delete v z))
       (print (multiple-value-list (fulfil v)))
       (print z)
       (print *promise-keeper*)
       (force-output *standard-output*)))

