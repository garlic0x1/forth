(in-package :forth)

(define-forth def
  (setf (quoted self) 'end)
  (push 'def (stack self)))

(define-forth end
  (setf (quoted self) nil)
  (let ((seq (pop-to 'def self)))
    (setf (gethash (car seq) (env self))
          (lambda (self)
            (let ((res))
              (dolist (word (cdr seq))
                (setf res (forth-eval self word)))
              res)))
    (car seq)))

(define-forth +
  (incf (cadr (stack self))
        (pop (stack self))))

(define-forth stack (stack self))

(define-forth env (env self))

(define-forth exit (setf (done self) t))

;; stack manipulation

(define-forth dup (push (car (stack self)) (stack self)))

(define-forth drop (pop (stack self)))

(define-forth swap
  (let ((head (car (stack self))))
    (setf (car (stack self)) (cadr (stack self))
          (cadr (stack self)) head)
    :ok))

(define-forth over (push (cadr (stack self)) (stack self)))

(define-forth rot
  (let ((a (first (stack self)))
        (b (second (stack self)))
        (c (third (stack self))))
    (setf (third (stack self)) b
          (second (stack self)) a
          (first (stack self)) c)
    :ok))

(define-forth print
  (format (output self) "~a " (pop (stack self)))
  :ok)
