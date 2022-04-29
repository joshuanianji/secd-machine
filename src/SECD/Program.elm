module SECD.Program exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Lib.Cons as Cons exposing (Cons)
import Lib.LispAST as AST exposing (AST, Token)
import Lib.Util as Util
import SECD.Error exposing (Error)



-- | SECD Code


type Program
    = Program (List Op)



-- possible operands in the SECD "language"


type Op
    = NIL
    | LD ( Int, Int ) -- Loads a value from context
    | LDC Int -- Loads a constant
    | LDF -- Loads a function
    | AP -- Applies a function
    | RTN -- Returns from a function
    | SEL -- Selects in an If statement
    | JOIN -- rejoin main control
    | RAP -- recursive apply
    | DUM -- Create a dummy env
    | FUNC Func -- builtin functions
    | NESTED (List Op)


type Func
    = ADD
    | MULT
    | SUB
    | ATOM -- true if the element is an atom (integer or boolean), NIL otherwise
    | CONS
    | CAR
    | CDR
    | NULL -- Returns true on Nil
    | COMPARE Cmp



-- prepending with CMP so we don't clash with the predefined comparison types


type Cmp
    = CMP_EQ
    | CMP_NE
    | CMP_LT
    | CMP_GT
    | CMP_LEQ
    | CMP_GEQ



-- turns a Compare to an Elm function that we can use.


cmpFunc : Cmp -> (comparable -> comparable -> Bool)
cmpFunc cmp =
    case cmp of
        CMP_EQ ->
            (==)

        CMP_NE ->
            (/=)

        CMP_LT ->
            (<)

        CMP_GT ->
            (>)

        CMP_LEQ ->
            (<=)

        CMP_GEQ ->
            (>=)



-- debug


opToString : Op -> String
opToString op =
    case op of
        NIL ->
            "NIL"

        LD ( i, j ) ->
            "LD (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")"

        LDC i ->
            "LDC " ++ String.fromInt i

        LDF ->
            "LDF"

        AP ->
            "AP"

        RTN ->
            "RTN"

        SEL ->
            "SEL"

        JOIN ->
            "JOIN"

        RAP ->
            "RAP"

        DUM ->
            "DUM"

        FUNC f ->
            funcToString f

        NESTED ops ->
            "[" ++ (String.join ", " <| List.map opToString ops) ++ "]"


funcToString : Func -> String
funcToString f =
    case f of
        ADD ->
            "+"

        MULT ->
            "*"

        SUB ->
            "-"

        ATOM ->
            "ATOM"

        CONS ->
            "CONS"

        CAR ->
            "CAR"

        CDR ->
            "CDR"

        NULL ->
            "NULL"

        COMPARE cmp ->
            cmpToString cmp


cmpToString : Cmp -> String
cmpToString cmp =
    case cmp of
        CMP_EQ ->
            "=="

        CMP_NE ->
            "!="

        CMP_LT ->
            "<"

        CMP_GT ->
            ">"

        CMP_LEQ ->
            "<="

        CMP_GEQ ->
            ">="



-- views


view : Op -> Html msg
view op =
    case op of
        NIL ->
            Html.div [ Attr.class "vm-op nil" ] [ Html.text "NIL" ]

        LD ( i, j ) ->
            Html.div [ Attr.class "vm-op ld" ] [ Html.text <| "LD (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")" ]

        LDC i ->
            Html.div [ Attr.class "vm-op ldc" ] [ Html.text <| "LDC " ++ String.fromInt i ]

        LDF ->
            Html.div [ Attr.class "vm-op lfc" ] [ Html.text <| "LDF" ]

        AP ->
            Html.div [ Attr.class "vm-op ap" ] [ Html.text <| "AP" ]

        RTN ->
            Html.div [ Attr.class "vm-op rtn" ] [ Html.text <| "RTN" ]

        SEL ->
            Html.div [ Attr.class "vm-op sel" ] [ Html.text <| "SEL" ]

        JOIN ->
            Html.div [ Attr.class "vm-op join" ] [ Html.text <| "JOIN" ]

        RAP ->
            Html.div [ Attr.class "vm-op rap" ] [ Html.text <| "RAP" ]

        DUM ->
            Html.div [ Attr.class "vm-op dum" ] [ Html.text <| "DUM" ]

        FUNC f ->
            Html.div [ Attr.class "vm-op func" ] [ Html.text <| funcToString f ]

        NESTED ops ->
            List.map view ops
                |> List.intersperse (Html.text ",")
                |> Util.wrapAdd (Html.text "[") (Html.text "]")
                |> Html.div [ Attr.class "vm-op nested row" ]



-- consumable by the SECD VM


toList : Program -> List Op
toList (Program ops) =
    ops


fromList : List Op -> Program
fromList =
    Program



-- useful in testing


fromSingleton : Op -> Program
fromSingleton op =
    Program [ op ]



---- COMPILATION ----


compile : AST -> Result Error Program
compile ast =
    Result.map Program <| compile_ emptyEnv ast


compile_ : Environment -> AST -> Result Error (List Op)
compile_ env ast =
    case ast of
        -- nil vals
        AST.Var (AST.Token "nil") ->
            Ok [ NIL ]

        -- truthy
        -- don't want to add another SECD operand, and this solved the issue of adding a True to the top of the stack
        AST.Truthy ->
            Ok [ NIL, FUNC ATOM ]

        AST.Var (AST.Token var) ->
            Result.map List.singleton <| lookup var env

        -- Compile list (nil on empty list)
        AST.Quote cons ->
            Ok <| compileCons cons

        AST.Val n ->
            Ok <| [ LDC n ]

        AST.FuncApp f args ->
            compileFuncApp env f args False

        AST.If cond branchT branchF ->
            Result.map3
                (\compiledCond compiledT compiledF ->
                    compiledCond ++ [ SEL, NESTED (compiledT ++ [ JOIN ]), NESTED (compiledF ++ [ JOIN ]) ]
                )
                (compile_ env cond)
                (compile_ env branchT)
                (compile_ env branchF)

        AST.Lambda vars body ->
            compileLambda env vars body

        AST.Let bindings body ->
            compileLet env bindings body

        AST.Letrec bindings body ->
            compileLetrec env bindings body


compileCons : Cons Int -> List Op
compileCons cs =
    let
        compileConsHelper : Cons Int -> List Op -> List Op
        compileConsHelper cons acc =
            case cons of
                Cons.Nil ->
                    NIL :: acc

                Cons.Val n ->
                    [ LDC n ]

                Cons.Cons hd tl ->
                    compileConsHelper tl <| compileCons hd ++ (FUNC CONS :: acc)
    in
    compileConsHelper cs []



-- compiles a function being called with the arguments


compileFuncApp : Environment -> AST -> List AST -> Bool -> Result Error (List Op)
compileFuncApp env f args isRecursive =
    let
        apCall =
            if isRecursive then
                RAP

            else
                AP

        checkArity : ( Maybe Int, List Op, Bool ) -> Result Error ( List Op, Bool )
        checkArity ( mArity, compiledFunction, isBuiltin ) =
            if mArity == Just (List.length args) || mArity == Nothing then
                if isBuiltin then
                    Ok ( compiledFunction, isBuiltin )

                else
                    Ok ( compiledFunction ++ [ apCall ], isBuiltin )

            else
                Err <| "Invalid number of arguments! Expecting: " ++ Util.showMaybeInt mArity ++ ", and got :" ++ String.fromInt (List.length args)

        addArguments : ( List Op, Bool ) -> Result Error (List Op)
        addArguments ( compiledFunction, isBuiltin ) =
            Result.map
                (\compiledArgs ->
                    compiledArgs ++ compiledFunction
                )
                (compileArgs env isBuiltin args)
    in
    compileFunc env f
        |> Result.andThen checkArity
        |> Result.andThen addArguments


compileLambda : Environment -> List Token -> AST -> Result Error (List Op)
compileLambda env vars body =
    let
        newEnv =
            addVarNames (List.map AST.tokenToStr vars) env

        compiledBody =
            compile_ newEnv body
    in
    Result.map
        (\compiledBodyOk -> [ LDF, NESTED (compiledBodyOk ++ [ RTN ]) ])
        compiledBody



-- A let function can be thought of as a lambda function application!


compileLet : Environment -> List ( Token, AST ) -> AST -> Result Error (List Op)
compileLet =
    compileLetHelper False


compileLetrec : Environment -> List ( Token, AST ) -> AST -> Result Error (List Op)
compileLetrec =
    compileLetHelper True



-- extrapolates the similaries with compiling let and letrec


compileLetHelper : Bool -> Environment -> List ( Token, AST ) -> AST -> Result Error (List Op)
compileLetHelper isRecursive env bindings body =
    let
        vars =
            List.map Tuple.first bindings

        -- the values, or the bound values, can be thought of as "arguments"
        vals =
            List.map Tuple.second bindings

        newEnv =
            addVarNames (List.map AST.tokenToStr vars) env

        lambdaBody =
            AST.Lambda vars body
    in
    if isRecursive then
        -- add DUM in front
        Result.map ((::) DUM) <| compileFuncApp newEnv lambdaBody vals isRecursive

    else
        compileFuncApp env lambdaBody vals isRecursive



-- for Builtin functions, we load the arguments onto the stack naively
-- for lambda/loaded functions, we load the arguments onto the stack in a list
-- for loaded functions, we also do not run LDF, but run AP right after


type FuncType
    = BuiltinFunc
    | LambdaFunc
    | LoadedFunc



-- Compiles the function that we're calling arguments with
-- Returns : ( Arity, Compiled Function, FuncType )
-- the Arity is Nothing if we don't know how many arguments it takes (e.g. an if statement)


compileFunc : Environment -> AST -> Result Error ( Maybe Int, List Op, Bool )
compileFunc env f =
    case f of
        -- Builtin functions
        AST.Var (AST.Token "+") ->
            Ok <| ( Just 2, [ FUNC ADD ], True )

        AST.Var (AST.Token "*") ->
            Ok <| ( Just 2, [ FUNC MULT ], True )

        AST.Var (AST.Token "-") ->
            Ok <| ( Just 2, [ FUNC SUB ], True )

        AST.Var (AST.Token "atom") ->
            Ok <| ( Just 1, [ FUNC ATOM ], True )

        AST.Var (AST.Token "cons") ->
            Ok <| ( Just 2, [ FUNC CONS ], True )

        AST.Var (AST.Token "car") ->
            Ok <| ( Just 1, [ FUNC CAR ], True )

        AST.Var (AST.Token "cdr") ->
            Ok <| ( Just 1, [ FUNC CDR ], True )

        AST.Var (AST.Token "null") ->
            Ok <| ( Just 1, [ FUNC NULL ], True )

        AST.Var (AST.Token "eq") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_EQ) ], True )

        AST.Var (AST.Token "<") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_LT) ], True )

        AST.Var (AST.Token ">") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_GT) ], True )

        AST.Var (AST.Token "<=") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_LEQ) ], True )

        AST.Var (AST.Token ">=") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_GEQ) ], True )

        -- custom function
        -- Find the definition in the environment
        AST.Var (AST.Token token) ->
            lookup token env
                |> Result.map (\val -> ( Nothing, [ val ], False ))

        AST.Val _ ->
            Err "Illegal function call - attempting to call an integer!"

        AST.Quote _ ->
            Err "Illegal function call - attempting to call a quote value!"

        AST.Truthy ->
            Err "Illegal function call - attempting to call a truthy value!"

        -- do NOT allow automatic currying
        AST.FuncApp f_ args_ ->
            let
                checkArity : ( Maybe Int, List Op, Bool ) -> Result Error ( Maybe Int, List Op, Bool )
                checkArity ( funcArity, compiledFunc, isBuiltin ) =
                    if funcArity == Just (List.length args_) then
                        Ok ( Just 0, compiledFunc ++ [ AP ], isBuiltin )

                    else if funcArity == Nothing then
                        Ok ( Nothing, compiledFunc ++ [ AP ], isBuiltin )

                    else
                        Err "Incorrect number of arguments in function application!"

                addArgs : ( Maybe Int, List Op, Bool ) -> Result Error ( Maybe Int, List Op, Bool )
                addArgs ( funcArity, compiledFunc, isBuiltin ) =
                    Result.map
                        (\compiledArgs -> ( funcArity, compiledArgs ++ compiledFunc, isBuiltin ))
                        (compileArgs env isBuiltin args_)
            in
            -- Compiles the function, then compiles arguments
            -- argument compilation depends on if the function is builtin (push arguments onto stack naively)
            -- or not (create a list to push onto the stack)
            compileFunc env f_
                |> Result.andThen checkArity
                |> Result.andThen addArgs

        AST.Lambda args f_ ->
            compileLambda env args f_
                |> Result.map (\ops -> ( Just <| List.length args, ops, False ))

        _ ->
            Err "Compile Function - not implemented yet."



-- ifthe arguments are from a builtin function, we can just push them onto the stack
-- otherwise, make a list out of the arguments.


compileArgs : Environment -> Bool -> List AST -> Result Error (List Op)
compileArgs env isBuiltin args =
    let
        compileNonBuiltin : List AST -> Result Error (List Op)
        compileNonBuiltin args_ =
            case args_ of
                [] ->
                    Ok []

                arg :: xs ->
                    Result.map2
                        (\cmpArg cmpArgs -> cmpArg ++ FUNC CONS :: cmpArgs)
                        (compile_ env arg)
                        (compileNonBuiltin xs)

        compileBuiltin : List AST -> Result Error (List Op)
        compileBuiltin args_ =
            case args_ of
                [] ->
                    Ok []

                arg :: xs ->
                    Result.map2
                        (\cmpArg cmpArgs -> cmpArg ++ cmpArgs)
                        (compile_ env arg)
                        (compileBuiltin xs)
    in
    if List.length args == 0 then
        Ok []

    else if isBuiltin then
        -- we want to push the arguments on the stack in REVERSE order
        compileBuiltin <| List.reverse args

    else
        -- appending "NIL" to the front
        Result.map ((::) NIL) (compileNonBuiltin <| List.reverse args)



---- ENVIRONMENT ----
-- keeps track of the environment, or the value of a variable
-- when we immediately come across a variable name we don't know, we exit, unlike lisp


type Environment
    = -- doubly nested list of argument names
      -- Used to calculate the "coordinates" of each value
      Env (List (List String))


emptyEnv : Environment
emptyEnv =
    Env []



-- returns an apprioriate LD (x.y), else fails with "unknown variable"


lookup : String -> Environment -> Result Error Op
lookup var (Env env) =
    let
        searchLine : List String -> Int -> Int -> Maybe Op
        searchLine l y x =
            case l of
                [] ->
                    Nothing

                hd :: tl ->
                    if hd == var then
                        Just <| LD ( y, x )

                    else
                        searchLine tl y (x + 1)

        helper : List (List String) -> Int -> Result Error Op
        helper l y =
            case l of
                [] ->
                    Err <| "Unknown variable " ++ var ++ "!"

                hd :: tl ->
                    case searchLine hd y 0 of
                        Just op ->
                            Ok op

                        Nothing ->
                            helper tl (y + 1)
    in
    helper env 0



-- only add names/strings to the let bindings


addVarNames : List String -> Environment -> Environment
addVarNames vars (Env env) =
    Env (vars :: env)
