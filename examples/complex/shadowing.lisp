; Shadowing is generally discouraged, but it works in this language!
; It means a variable can be redefined in an inner scope.
; We'll look at the innermost scope when we look up the value of a variable.
;
; In the example, "x" and "w" are defined multiple times.
; Take a look at the compiled code to see which "x" is used in which scope!
(let 
    ((x 1)) 
    (let 
        ((w (+ x 2))        ; x = 1 in this scope
         (x 5))
        (+ x w)))           ; x = 5 in this scope
