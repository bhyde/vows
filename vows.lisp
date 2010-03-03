;; -*- mode: lisp -*-

(in-package "VOWS")

(defclass vow ()
  ((run-state :initform :new :type (member :new :enqueued :keeping :kept :failed ))
   (lock :initform (bt:make-lock))
   (lambda :initarg :lambda)
   (results :initform nil :type list)
   (error? :initform nil :type (or null condition))
   (waiting :initform nil :type list)))

(defun fulfiled? (vow)
  (with-slots (run-state) vow
    (or (eq :kept run-state)
        (eq :failed run-state))))

(defmethod add-waiter (vow lock notify)
  (with-slots (waiting) vow
    (push (cons lock notify) waiting)))

(defmethod remove-waiter (vow lock notify)
  (with-slots (waiting) vow
    (setf waiting (delete t waiting :test #'(lambda (x y)
                                              (declare (ignore x))
                                              (and (eq lock (car y))
                                                   (eq notify (cdr y))))))))

(defmethod print-object ((v vow) stream)
  (with-slots (run-state) v
    (print-unreadable-object (v stream :type t :identity t)
      (format stream "~A" run-state))))

(defclass promise-keeper ()
  ((lock :initform (bt:make-lock))
   (fresh-promises :initform (bt:make-condition-variable))
   (promises :accessor promises :initform nil :type list)
   (threads :initform nil :type list)
   (max-threads :initform 4 :type fixnum)))

(defmethod print-object ((k promise-keeper) stream)
  (with-slots (threads promises) k
    (print-unreadable-object (k stream :type t :identity t)
      (format stream "~D threads ~D promises" (length threads) (length promises)))))

(defmethod keep-vow ((vow vow))
  (with-slots (lock lambda results error? run-state waiting) vow
    (bt:with-lock-held (lock)
      (setf run-state :keeping)
      (handler-case
          (progn
            (setf results
                  (multiple-value-list
                   (funcall lambda)))
            (setf run-state :kept))
        (condition (c)
          (setf results :error)
          (setf error? c)
          (setf run-state :failed))))
    (loop for (other-lock . notify) in waiting
       do (bt:condition-notify notify))))

(defmethod enqueue ((vow vow) (keeper promise-keeper))
  "Add a single vow to the promises of a promise keeper."
  (with-slots (fresh-promises promises (keeper-lock lock)) keeper
    (with-slots (run-state (vow-lock lock)) vow
      (bt:with-lock-held (keeper-lock)
        (bt:with-lock-held (vow-lock)
          (push vow promises)
          (setf run-state :enqueued))))
    (bt:condition-notify fresh-promises))
  vow)

(defun worker-of-promise-keeper (keeper)
  (with-slots (lock promises fresh-promises) keeper
    (loop
       (loop
          for vow? = (bt:with-lock-held (lock) (pop promises))
          while vow? 
          do (keep-vow vow?))
       (bt:with-lock-held (lock)
         (bt:condition-wait fresh-promises lock)))))


(defmethod initialize-instance :after ((keeper promise-keeper) &rest initargs &key &allow-other-keys)
  "Assure that the promise-keeper has an approprate number of threads running."
  (declare (ignore initargs))
  (with-slots (lock threads max-threads) keeper
    (bt:with-lock-held (lock)
      (loop for i below max-threads
         do
         (push (bt:make-thread 
                #'(lambda () (worker-of-promise-keeper keeper))
                :name (format nil "Thread ~d of promise keeper" i))
               threads)))))


(defvar *promise-keeper* (make-instance 'promise-keeper))

(defun vow (lambda)
  (enqueue (make-instance 'vow :lambda lambda) *promise-keeper*))

(defmethod fulfil ((vow vow))
  (with-slots (lock run-state results error?) vow
    (bt:with-lock-held (lock)
      (labels ((extract-result ()
                 (case run-state
                   (:kept
                    (values-list results))
                   (:failed
                    (error error?))
                   (otherwise
                    (let ((mylock (bt:make-lock))
                          (notify (bt:make-condition-variable)))
                      (add-waiter vow mylock notify)
                      (bt:condition-wait notify lock))
                    (extract-result)))))
        (extract-result)))))

(defun find-fulfiled (vows)
  "Return the first of vows that has been fulfilled."
  (let* ((our-lock (bt:make-lock))
         (got-one (bt:make-condition-variable))
         (waiting-upon ()))
    (unwind-protect
         (block :finished-vow
           (bt:with-lock-held (our-lock)
             ;; see if something is ready and setup a slew of notifications.
             (loop 
                for v in vows
                do
                (with-slots (lock) v
                  (bt:with-lock-held (lock)
                    (when (fulfiled? v) (return-from :finished-vow v))
                    (push v waiting-upon)
                    (add-waiter v our-lock got-one))))
             (loop 
                (loop for v in vows
                   when (fulfiled? v)
                   do (return-from :finished-vow v))
                (bt:condition-wait got-one our-lock))))
      (loop for v in waiting-upon
           do (with-slots (lock) v
                (bt:with-lock-held (lock)
                  (remove-waiter v our-lock got-one)))))))


