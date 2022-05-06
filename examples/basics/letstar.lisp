(letrec
    ((x 1) 
     (w (lambda () (+ z 1))) 
     (y 2)
     (z 3)) 
    (+ (+ x y) (+ (w) z)))