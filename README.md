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

Lisp interop:

```forth
> 1 (+ 2 3) + print
6 OK
> (uiop:hostname) stack
(garlic)
```
