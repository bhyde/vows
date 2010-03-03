;; -*- mode: common-lisp -*-

(in-package "VOWS")

(def-suite vows :description "Testing for vows.")

(in-suite vows)

(test zero-test
  "The simplest test"
  (is (eq t t)))

(test one-promise
  "One promise"
  (is (eq 1 (fulfil (vow #'(lambda () 1))))))

(test two-promises
  (is (eq 4 (+ (fulfil (vow #'(lambda () 1))) (fulfil (vow #'(lambda () 3)))))))

(test two-promises-with-sleep
  (is (eq 4 (+ (fulfil (vow #'(lambda () (sleep 2)  1))) (fulfil (vow #'(lambda () (sleep 1) 3)))))))

(test herd-of-promises
  (let ((n 20))
    (labels ((cow (n)
               #'(lambda ()
                   ; (sleep (random 3))
                   n))
             (make-herd ()
               (loop for i below n collect (vow (cow i)))))
      (is (eq (loop for i below n sum i)
              (loop with herd = (make-herd)
                 while herd
                 as cow = (find-fulfiled herd)
                 do (setf herd (delete cow herd))
                 sum (fulfil cow)))))))
