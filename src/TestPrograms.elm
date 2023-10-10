module TestPrograms exposing (factorial, fibonacci, mutuallyRecursiveIsEven, recursiveLength)

-- | List of programs that test suites can use.


mutuallyRecursiveIsEven : Int -> String
mutuallyRecursiveIsEven n =
    "(letrec ((odd (lambda (n) (if (eq n 0) nil (even (- n 1))))) (even (lambda (n) (if (eq n 0) (atom nil) (odd (- n 1)))))) (even " ++ String.fromInt n ++ "))"


recursiveLength : String
recursiveLength =
    "(letrec ((f (lambda (x m) (if (null x) m (f (cdr x) (+ m 1)))))) (f '(1 2 3) 0))"


factorial : Int -> String
factorial n =
    let
        factDef =
            "lambda (n) (if (eq n 0) 1 (* n (fact (- n 1))))"

        factApp =
            "fact " ++ String.fromInt n
    in
    "(letrec ((fact (" ++ factDef ++ "))) (" ++ factApp ++ "))"


fibonacci : Int -> String
fibonacci n =
    let
        fibDef =
            "lambda (n) (if (eq n 0) 0 (if (eq n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))"
    in
    "(letrec ((fib (" ++ fibDef ++ "))) (fib " ++ String.fromInt n ++ "))"
