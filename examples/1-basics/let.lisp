---
title: Let Statements
---
; Check how the let binding is compiled to a lambda application!
(let ((w 5) (x 1)) 
    (let ((y 2))
        (let ((z 3)) (+ (+ x y) (+ w z)))))