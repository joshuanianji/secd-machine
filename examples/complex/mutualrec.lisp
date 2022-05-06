
(letrec
    ((odd 	(lambda (n) (if (eq n 0) nil (even (- n 1)))))
     (even 	(lambda (n) (if (eq n 0) t	 (odd  (- n 1)))))) 
    (even 4))