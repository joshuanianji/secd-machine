---
name: Length of a List - Recursion
---
(letrec
    ((f (lambda (x m) (if (null x) m (f (cdr x) (+ m 1))))))
    (f '(1 2 3) 0))
