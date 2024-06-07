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

(defmacro forth-eval-symbol (self obj)
  `(if (and (quoted ,self) (not (eql ,obj (quoted ,self))))
       (progn (push ,obj (stack ,self)) :ok)
       (if-let ((value (gethash ,obj (env ,self))))
         (funcall value ,self)
         :?)))

(defmacro forth-eval-primitive (self obj)
  `(push ,obj (stack ,self)))

(defmacro forth-eval-list (self obj)
  `(push (eval ,obj) (stack ,self)))

(defmacro forth-eval-vector (self obj)
  `(push (lambda (self)
           (loop :for word :across ,obj
                 :do (forth-eval self word)))
         (stack ,self)))

(defgeneric forth-eval (self obj)
  (:method (self (obj symbol)) (forth-eval-symbol self obj))
  (:method (self (obj list)) (forth-eval-list self obj))
  (:method (self (obj vector)) (forth-eval-vector self obj))
  (:method (self (obj string)) (forth-eval-primitive self obj))
  (:method (self (obj t)) (forth-eval-primitive self obj)))

(defun forth-eval-words (self words)
  (let ((res))
    (dolist (word words) (setf res (forth-eval self word)))
    res))

(defun run-forth ()
  (loop :with forth := (make-instance 'forth) :until (done forth)
        :do (format (output-err forth) "> ")
            (let* ((line (read-line (input forth)))
                   (words (read-from-string (format nil "(~a)" line)))
                   (out (forth-eval-words forth words)))
              (format (output-err forth) "~a~%" out))))
