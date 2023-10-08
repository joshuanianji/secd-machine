; currying does not work natively, so we have to use lambdas
; here, curry2 "creates" a curried version of a 2-ary function
(let
    ((curry2 (lambda (f) (lambda (x) (lambda (y) (f x y)))))
     ; turning 2-ary function into "curry-able" versions using lambdas
     (add (lambda (x y) (+ x y)))
     (mult (lambda (x y) (* x y)))
     (sub (lambda (x y) (- x y))))
    ; Change the function names to any of the above, or create your own!
    (((curry2 add) 5) 10))
