---
title: Simulating Let*
---
; Our language does not support Lisp's let*
; In a let* binding, the variables are evaluated sequentially, so variables further down the list can reference variables earlier on.
; 
; The let binding does not allow us to reference variables in the same binding scope. Usually, we would need to nest let bindings.
; To "solve" this, can use a letrec binding, but wrap references in lambdas. For JS users, this is sometimes called a "thunk" (i think???).
; Anyway, we'll have to "evaluate the thunk" when referencing the variable in the let value.
(letrec
    ((x 1) 
     (w (lambda () (+ z 1))) 
     (y 2)
     (z 3)) 
    (+ (+ x y) (+ (w) z)))