(in-package :forth)

(defmacro compile-forth-word (self obj)
  (cond ((symbolp obj)
         `(forth-eval-symbol ,self ,obj))
        ((stringp obj)
         `(forth-eval-primitive ,self ,obj))
        ((listp obj)
         `(forth-eval-list ,self ,obj))
        ((vectorp obj)
         `(forth-eval-vector ,self ,obj))
        (t
         `(forth-eval-primitive ,self ,obj))))

(defmacro compile-forth (&body body)
  `(lambda (self)
     ,@(mapcar (lambda (word) `(compile-forth-word self ,word))
               body)))
