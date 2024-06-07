(in-package :forth)

(defun pop-to (word forth &optional acc)
  "Utility for popping quoted forms."
  (let ((head (pop (stack forth))))
    (if (or (null head) (eql word head))
        acc
        (pop-to word forth (cons head acc)))))

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

(define-forth if
  (setf (quoted self) 'then)
  (push 'if (stack self)))

(define-forth then
  (setf (quoted self) nil)
  (let* ((words (pop-to 'if self))
         (split-point (search (list 'else) words))
         (res))
    (if (pop (stack self))
        (dolist (word (subseq words 0 (or split-point (length words))))
          (setf res (forth-eval self word)))
        (when split-point
          (dolist (word (subseq words split-point (length words)))
            (setf res (forth-eval self word)))))
    res))

(define-forth do
  (setf (quoted self) 'loop)
  (push 'do (stack self)))

(define-forth loop
  (setf (quoted self) nil)
  (let ((body (pop-to 'do self))
        (start (pop (stack self)))
        (end (pop (stack self))))
    (loop :for i :from start :to (1- end)
          :do (setf (gethash 'i (env self)) (lambda (self) (forth-eval self i)))
              (dolist (word body) (forth-eval self word)))
    :ok))

(define-forth call
  (funcall (pop (stack self)) self))

(define-forth stack (stack self))

(define-forth env (env self))

(define-forth exit (setf (done self) t))

(define-forth dup (push (car (stack self)) (stack self)))

(define-forth drop (pop (stack self)))

(define-forth clear (setf (stack self) '()))

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

(define-forth emit
  (format (output self) "~a" (code-char (pop (stack self)))))

(define-forth cr
  (format (output self) "~a~a" #\Return #\Newline))

(define-forth =
  (push (= (pop (stack self)) (pop (stack self)))
        (stack self)))

(define-forth equal
  (push (equal (pop (stack self)) (pop (stack self)))
        (stack self)))

(define-forth <
  (push (< (pop (stack self)) (pop (stack self))) (stack self)))

(define-forth >
  (push (> (pop (stack self)) (pop (stack self))) (stack self)))

(define-forth invert
  (setf (car (stack self)) (not (car (stack self)))))

(define-forth +
  (incf (cadr (stack self))
        (pop (stack self))))

(define-forth -
  (decf (cadr (stack self))
        (pop (stack self))))

(define-forth *
  (setf (cadr (stack self))
        (* (cadr (stack self)) (pop (stack self)))))

(define-forth /
  (setf (cadr (stack self))
        (/ (cadr (stack self)) (pop (stack self)))))

(define-forth mod
  (let ((divisor (pop (stack self))))
    (setf (car (stack self)) (mod (car (stack self)) divisor))))
