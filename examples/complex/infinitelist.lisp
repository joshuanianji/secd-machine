; Using lazy evaluation, we can create a an "infinite list" generator
(letrec 
    ((add1 (lambda (x) (+ x 1)))
     (mapStream (lambda (f s) (cons (f (car (s))) (lambda () (mapStream f (cdr (s)))))))
     ; `stream1` generates constant `1` values
     (stream1 (lambda () (cons 1 stream1)))
     ; `stream2` generates natural numbers, starting from 1.
     (stream2 (lambda () (cons 1 (lambda () (mapStream add1 stream2)))))
	; On the other hand, `takeN` takes `n` numbers from a stream by evaluating the stream at each point
     (takeN (lambda (n s) 
        (if (eq n 0) 
            nil
            (let
                ((tookN (lambda () (takeN (- n 1) (cdr (s))))))
                 (cons (car (s)) (tookN)))))))
    (takeN 4 stream2))
