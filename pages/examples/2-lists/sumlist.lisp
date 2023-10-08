---
title: Summing a List
---
; here, we're summing a list using two different methods
; One directly, and one using foldr as a helper function
(letrec 
     ; Method one: direct
    ((sumlistDirect (lambda (l) (if (null l) 0 (+ (car l) (sumlist (cdr l))))))
    
     ; Method two: using foldr
     (foldr (lambda (f z l) (if (null l) z (foldr f (f z (car l)) (cdr l)))))
     (add (lambda (x y) (+ x y)))
     (sumlist (lambda (l) (foldr add 0 l))))

    ; update the line from calling `sumlistDirect` to `sumList`!
    ; note how both function calls produce the same value
    (sumlist '(1 2 3)) 
)
