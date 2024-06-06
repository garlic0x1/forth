(in-package :forth)

(defvar *base-env* (make-hash-table))

(defmacro define-forth (symbol &body body)
  "Define a forth word as a Lisp procedure."
  `(setf (gethash (quote ,symbol) *base-env*)
         (lambda (self)
           (declare (ignorable self))
           ,@body)))

(defclass forth ()
  ((stack
    :initarg :stack
    :initform '()
    :accessor stack)
   (env
    :initarg :env
    :initform *base-env*
    :accessor env)
   (input
    :initarg :input
    :initform *standard-input*
    :accessor input)
   (output
    :initarg :output
    :initform *standard-output*
    :accessor output)
   (output-err
    :initarg :output-err
    :initform *debug-io*
    :accessor output-err)
   (quoted
    :initform nil
    :accessor quoted)
   (done
    :initform nil
    :accessor done)))

(defmethod forth-eval (self (obj number))
  (push obj (stack self))
  :ok)

(defmethod forth-eval (self (obj symbol))
  (if (and (quoted self) (not (eql obj (quoted self))))
      (progn
        (push obj (stack self))
        :ok)
      (if-let ((value (gethash obj (env self))))
        (funcall value self)
        :?)))

(defmethod forth-eval (self (obj string))
  (push obj (stack self))
  :ok)

(defmethod forth-eval (self (obj list))
  (push (eval obj) (stack self)))

(defun forth-eval-words (self words)
  (let ((res))
    (dolist (word words) (setf res (forth-eval self word)))
    res))

(defmethod run-forth ()
  (let ((forth (make-instance 'forth)))
    (format (output-err forth) "> ")
    (loop :until (done forth)
          :for line := (read-line (input forth))
          :for words := (read-from-string (format nil "(~a)" line))
          :for out := (forth-eval-words forth words)
          :do (format (output-err forth) "~a~%" out)
          :do (format (output-err forth) "> "))))
