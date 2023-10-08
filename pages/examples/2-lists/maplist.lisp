---
title: List Map
---
; here, we're defining a higher order function maplist, which takes in a function to apply to the elements
; the function is a lambda that adds 1 to a number
; Note that currying does not natively work in this language, we have to wrap the function in a lambda
(letrec 
    ((maplist (lambda (f l) (if (null l) nil (cons (f (car l)) (maplist f (cdr l))))))
     (add1 (lambda (x) (+ x 1))))
    (maplist add1 '(1 2 3)) 
)