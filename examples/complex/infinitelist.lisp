(letrec 
    ((add1 (lambda (x) (+ x 1)))
     (mapStream (lambda (f s) (cons (f (car (s))) (mapStream f (cdr (s))))))
     (stream1 (lambda () (cons 1 stream1)))
     (stream2 (lambda () (cons 1 (mapStream add1 stream2))))
     (takeN (lambda (n s) 
        (if (eq n 0) 
            nil
            (let
                ((tookN (lambda () (takeN (- n 1) (cdr (s))))))
                 (cons (car (s)) (tookN)))))))
    (takeN 4 stream1))
