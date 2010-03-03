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

(defgeneric keep-vow (t)
  (:documentation "The internal method on a vow that actually keeps the promise and fires the triggers."))

(defgeneric enqueue (t t)
  (:documentation "The internal method on of a promise keeper which adds a vow to it's to do list."))

(defgeneric break-vow (t)
  (:documentation "The external method used force a vow to be fufilled such that retrieve it signals the broken-vow error."))

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

(defun fire-triggers (vow)
  "Invoked once a vow has been fufilled it runs the vow's triggers."
  (with-slots (triggers) vow
    (loop
       for trigger? = (pop triggers)
       while trigger? do (funcall (cdr trigger?) vow))))

(defmethod print-object ((v vow) stream)
  (with-slots (run-state) v
    (print-unreadable-object (v stream :type t :identity t)
      (format stream "~A" run-state))))

(defclass promise-keeper ()
  ((lock :initform (bt:make-lock))
   (fresh-promises :initform (bt:make-condition-variable))
   (to-do-list :accessor to-do-list :initform nil :type list)
   (threads :initform nil :type list)
   (vow-escapes :initform (make-hash-table :test #'eq)
                :documentation "For every vow we are working maps to a clozure that will stop that and break the vow.")
   (max-threads :initform 4 :type fixnum)))

(defmethod print-object ((k promise-keeper) stream)
  (with-slots (threads to-do-list) k
    (print-unreadable-object (k stream :type t :identity t)
      (format stream "~D threads ~D to-do-list" (length threads) (length to-do-list)))))

(define-condition broken-vow (simple-error) ())

(defun mark-vow-as-broken (vow)
  (with-slots (run-state error?) vow
    (setf run-state :failed)
    (setf error? (make-condition 'broken-vow
                         :format-control "Broke the vow ~S"
                         :format-arguments (list vow)))))

(defmethod break-vow ((vow vow))
  (with-slots ((pk-lock lock) to-do-list vow-escapes) *promise-keeper*
    (with-slots ((vow-lock lock) run-state) vow
      (bt:with-lock-held (pk-lock)
        (bt:with-lock-held (vow-lock)
          (ecase run-state
            (:enqueued
             (setf to-do-list (delete vow to-do-list))
             (mark-vow-as-broken vow))
            (:keeping
             (funcall (gethash vow vow-escapes)))
            ((:new :kept :failed)
             (mark-vow-as-broken vow)))))))
  (fire-triggers vow))



(defmethod keep-vow ((vow vow))
  (with-slots (lock lambda results error? run-state) vow
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
          (setf run-state :failed)))))
  (fire-triggers vow))

(defmethod enqueue ((vow vow) (keeper promise-keeper))
  "Add a single vow to the to-do-list of a promise keeper."
  (with-slots (fresh-promises to-do-list (keeper-lock lock)) keeper
    (with-slots (run-state (vow-lock lock)) vow
      (bt:with-lock-held (keeper-lock)
        (bt:with-lock-held (vow-lock)
          (push vow to-do-list)
          (setf run-state :enqueued))))
    (bt:condition-notify fresh-promises))
  vow)

(defun worker-of-promise-keeper (keeper)
  (with-slots ((keeper-lock lock) to-do-list fresh-promises vow-escapes) keeper
    ;; run forever.
    (loop
       ;; Drain the to do list, keep our promises.  The awesome
       ;; complexity is so we can break them in the midst of keeping
       ;; them.
       (loop 
          named working-on-the-to-list
          with vow
          do (block :abandon-ship
               (flet ((abandon-vow ()
                        (mark-vow-as-broken vow)
                        (return-from :abandon-ship :broke-vow-while-running)))
                 (unwind-protect
                      (progn
                        (bt:with-lock-held (keeper-lock) 
                          (setf vow (pop to-do-list))
                          (unless vow (return-from working-on-the-to-list nil))
                          (setf (gethash vow vow-escapes) #'abandon-vow))
                        (keep-vow vow))
                   (bt:with-lock-held (keeper-lock)
                     (remhash vow vow-escapes))))))
       ;; Todo list is empty, so wait for fresh promises to keep.
       (bt:with-lock-held (keeper-lock)
         (bt:condition-wait fresh-promises keeper-lock)))))


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


