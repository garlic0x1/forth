(in-package :forth)

(defvar *base-env* (make-hash-table))

(defmacro define-forth (symbol &body body)
  `(setf (gethash (quote ,symbol) *base-env*)
         (lambda (self)
           (declare (ignorable self))
           ,@body)))

(defun pop-to (word forth &optional acc)
  (let ((head (pop (stack forth))))
    (if (or (null head) (eql word head))
        acc
        (pop-to word forth (cons head acc)))))

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
  (let ((res))
    (dolist (it obj) (setf res (forth-eval self it)))
    res))

(defmethod forth-eval (self obj)
  (error (format nil "Unknown type: ~a, type: ~a~%" obj (type-of obj))))

(defmethod run-forth ()
  (let ((forth (make-instance 'forth)))
    (format (output-err forth) "> ")
    (loop :until (done forth)
          :for line := (read-line (input forth))
          :for words := (read-from-string (format nil "(~a)" line))
          :for out := (forth-eval forth words)
          :do (format (output-err forth) "~a~%" out)
          :do (format (output-err forth) "> "))))
