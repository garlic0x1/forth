A simple Forth interpreter made for fun.

Run the REPL:

```lisp
(ql:quickload :forth)
(forth:run-forth)
```

FizzBuzz:

```forth
> def fizzbuzz 5 mod 0 = if "fizz" else "buzz" then end
FIZZBUZZ
> 16 4 do i fizzbuzz loop stack
(fizz buzz buzz buzz buzz fizz buzz buzz buzz buzz fizz buzz)
```

Lambdas can be placed on the stack with #(word1 word2):

```forth
> 1 #(1 +) stack
(#<FUNCTION (LAMBDA (SELF) :IN FORTH-EVAL) {10027FBF0B}> 1)
> call stack
(2)
```

Lisp interop:

```forth
> 1 (+ 2 3) + print
6 OK
> (uiop:hostname) stack
(garlic)
```

Native procedures can be implemented with `define-forth`, giving access to `self`

```lisp
(define-forth +
  (incf (cadr (stack self))
        (pop (stack self))))
```
