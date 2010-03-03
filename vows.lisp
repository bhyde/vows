;; -*- mode: lisp -*-

(in-package "VOWS")

(deftype trigger () 
  "Each trigger is given a # to denote it so they can be deleted.  While this number
is only unique with a given vow some effort is made to keep them from overlapping."
  'fixnum)

(defgeneric create-trigger (t t)
  (:documentation "Given a vow and a lambda install as a trigger on the vow's fulfilment, and return trigger id.  The caller must be holding the vow's lock."))

(defgeneric delete-trigger (t t)
  (:documentation "Given a vow and either the trigger id or the lambda delete it from the vow. The caller must be holding the vow's lock."))

(defgeneric keep-vow (t))

(defgeneric enqueue (t t))

(defgeneric fulfil (t))

(defvar *trigger-generator* 10000)

(defclass vow ()
  ((run-state :initform :new :type (member :new :enqueued :keeping :kept :failed ))
   (lock :initform (bt:make-lock))
   (lambda :initarg :lambda)
   (results :initform nil :type list)
   (error? :initform nil :type (or null condition))
   (max-trigger :initform (prog1
                              *trigger-generator*
                            (when (< (- most-positive-fixnum 10000) *trigger-generator*)
                              (setf *trigger-generator* 0))
                            (incf *trigger-generator* 1000)))
   (triggers :initform nil :type list)))

(defun fulfiled? (vow)
  (with-slots (run-state) vow
    (or (eq :kept run-state)
        (eq :failed run-state))))

(defmethod create-trigger (vow lambda)
  (with-slots (triggers max-trigger) vow
    (incf max-trigger)
    (push (cons max-trigger lambda) triggers)))

(defmethod delete-trigger ((vow vow) (trigger fixnum))
  (with-slots (triggers) vow
    (setf triggers (delete trigger triggers :key #'car))))

(defmethod delete-trigger ((vow vow) (trigger function))
  (with-slots (triggers) vow
    (setf triggers (delete trigger triggers :key #'cdr))))



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
  (with-slots (lock lambda results error? run-state triggers) vow
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
    (loop
       for trigger? = (pop triggers)
       while trigger? do (funcall (cdr trigger?) vow))))

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

(defun make-condition-trigger (lock condition)
  #'(lambda (vow)
      (declare (ignore vow))
      (bt:with-lock-held (lock)
        (bt:condition-notify condition))))

(defmethod fulfil ((vow vow))
  (with-slots (lock run-state results error?) vow
    (labels ((extract-result ()
               (return-from fulfil
                 (case run-state
                   (:kept
                    (values-list results))
                   (:failed
                    (error error?))))))
      (bt:with-lock-held (lock)
        (when (fulfiled? vow)
          (extract-result)))
      (let* ((my-lock (bt:make-lock))
             (my-notify (bt:make-condition-variable))
             (my-trigger (make-condition-trigger my-lock my-notify)))
        (bt:with-lock-held (lock)
          (create-trigger vow my-trigger))
        (bt:with-lock-held (my-lock)
          (bt:condition-wait my-notify my-lock))
        (bt:with-lock-held (lock)
          (extract-result))))))



(defun find-fulfiled (vows)
  "Return the first of vows that has been fulfilled."
  (let* ((our-lock (bt:make-lock))
         (got-one (bt:make-condition-variable))
         (our-trigger (make-condition-trigger our-lock got-one))
         (unfulfilled-vows ()))
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
                    (push v unfulfilled-vows)
                    (create-trigger v our-trigger))))
             (loop 
                (loop for v in vows
                   when (fulfiled? v)
                   do (return-from :finished-vow v))
                (bt:condition-wait got-one our-lock))))
      (loop for v in unfulfilled-vows
           do (with-slots (lock) v
                (bt:with-lock-held (lock)
                  (delete-trigger v our-trigger)))))))


