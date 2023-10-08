; Even and odd reference each other!
; NOTE: This is quite an inefficient way to determine if a number is even or odd.
(letrec
    ((isOdd 	(lambda (n) (if (eq n 0) nil (isEven (- n 1)))))
     (isEven 	(lambda (n) (if (eq n 0) t	 (isOdd  (- n 1)))))) 
    (isEven 4))
