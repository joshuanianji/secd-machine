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

        AST.Var (AST.Token var) ->
            Result.map List.singleton <| lookup var env

        -- Compile list (nil on empty list)
        AST.Quote cons ->
            Ok <| compileCons cons

        AST.Val n ->
            Ok <| [ LDC n ]

        AST.FuncApp f args ->
            compileFuncApp env f args

        AST.If cond branchT branchF ->
            Result.map3
                (\compiledCond compiledT compiledF ->
                    compiledCond ++ [ SEL, NESTED (compiledT ++ [ JOIN ]), NESTED (compiledF ++ [ JOIN ]) ]
                )
                (compile_ env cond)
                (compile_ env branchT)
                (compile_ env branchF)

        AST.Let bindings body ->
            compileLet env bindings body

        AST.Lambda vars body ->
            compileLambda env vars body

        _ ->
            Err "Not supported yet"


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



-- there is a lot of overlap from this function and the compileFunc case for FuncApp
-- Not sure how to merge them, but we're assuming this is the topmost level function call
-- so, the arity must be exactly equal


compileFuncApp : Environment -> AST -> List AST -> Result Error (List Op)
compileFuncApp env f args =
    let
        checkArity : ( Maybe Int, List Op, FuncType ) -> Result Error ( List Op, FuncType )
        checkArity ( mArity, compiledFunction, funcType ) =
            if mArity == Just (List.length args) || mArity == Nothing then
                case funcType of
                    BuiltinFunc ->
                        Ok ( compiledFunction, funcType )

                    LambdaFunc ->
                        Ok ( compiledFunction ++ [ AP ], funcType )

                    LoadedFunc ->
                        Ok ( compiledFunction ++ [ AP ], funcType )

            else
                Err <| "Invalid number of arguments"

        addArguments : ( List Op, FuncType ) -> Result Error (List Op)
        addArguments ( compiledFunction, funcType ) =
            Result.map
                (\compiledArgs ->
                    compiledArgs ++ compiledFunction
                )
                (compileArgs env funcType args)
    in
    compileFunc env f
        |> Result.andThen checkArity
        |> Result.andThen addArguments



-- A let function can be thought of as a lambda function application!


compileLet : Environment -> List ( Token, AST ) -> AST -> Result Error (List Op)
compileLet env bindings body =
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
    compileFuncApp newEnv lambdaBody vals


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


compileFunc : Environment -> AST -> Result Error ( Maybe Int, List Op, FuncType )
compileFunc env f =
    case f of
        -- Builtin functions
        AST.Var (AST.Token "+") ->
            Ok <| ( Just 2, [ FUNC ADD ], BuiltinFunc )

        AST.Var (AST.Token "*") ->
            Ok <| ( Just 2, [ FUNC MULT ], BuiltinFunc )

        AST.Var (AST.Token "-") ->
            Ok <| ( Just 2, [ FUNC SUB ], BuiltinFunc )

        AST.Var (AST.Token "atom") ->
            Ok <| ( Just 1, [ FUNC ATOM ], BuiltinFunc )

        AST.Var (AST.Token "cons") ->
            Ok <| ( Just 2, [ FUNC CONS ], BuiltinFunc )

        AST.Var (AST.Token "car") ->
            Ok <| ( Just 1, [ FUNC CAR ], BuiltinFunc )

        AST.Var (AST.Token "cdr") ->
            Ok <| ( Just 1, [ FUNC CDR ], BuiltinFunc )

        AST.Var (AST.Token "null") ->
            Ok <| ( Just 1, [ FUNC NULL ], BuiltinFunc )

        AST.Var (AST.Token "eq") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_EQ) ], BuiltinFunc )

        AST.Var (AST.Token "<") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_LT) ], BuiltinFunc )

        AST.Var (AST.Token ">") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_GT) ], BuiltinFunc )

        AST.Var (AST.Token "<=") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_LEQ) ], BuiltinFunc )

        AST.Var (AST.Token ">=") ->
            Ok <| ( Just 2, [ FUNC (COMPARE CMP_GEQ) ], BuiltinFunc )

        -- custom function
        -- Find the definition in the environment
        AST.Var (AST.Token token) ->
            lookup token env
                |> Result.map (\val -> ( Nothing, [ val ], LoadedFunc ))

        AST.Val _ ->
            Err "Illegal function call - attempting to call an integer!"

        AST.Quote _ ->
            Err "Illegal function call - attempting to call a quote value!"

        -- do NOT allow currying
        AST.FuncApp f_ args_ ->
            let
                checkArity : ( Maybe Int, List Op, FuncType ) -> Result Error ( Maybe Int, List Op, FuncType )
                checkArity ( funcArity, compiledFunc, funcType ) =
                    if funcArity == Just (List.length args_) then
                        Ok ( Just 0, compiledFunc, funcType )

                    else
                        Err "Incorrect number of arguments in function application!"

                addArgs : ( Maybe Int, List Op, FuncType ) -> Result Error ( Maybe Int, List Op, FuncType )
                addArgs ( funcArity, compiledFunc, funcType ) =
                    Result.map
                        (\compiledArgs -> ( funcArity, compiledArgs ++ compiledFunc, funcType ))
                        (compileArgs env funcType args_)
            in
            -- Compiles the function, then compiles arguments
            -- argument compilation depends on if the function is builtin (push arguments onto stack naively)
            -- or not (create a list to push onto the stack)
            compileFunc env f_
                |> Result.andThen checkArity
                |> Result.andThen addArgs

        AST.Lambda args f_ ->
            compileLambda env args f_
                |> Result.map (\ops -> ( Just <| List.length args, ops, LambdaFunc ))

        -- idk how do compile user defined functions yet
        _ ->
            Err "Compile Function - not implemented yet."



-- ifthe arguments are from a builtin function, we can just push them onto the stack
-- otherwise, make a list out of the arguments.


compileArgs : Environment -> FuncType -> List AST -> Result Error (List Op)
compileArgs env funcType args =
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

    else if funcType == BuiltinFunc then
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
                    case searchLine hd y 1 of
                        Just op ->
                            Ok op

                        Nothing ->
                            helper tl (y + 1)
    in
    helper env 1



-- only add names/strings to the let bindings


addVarNames : List String -> Environment -> Environment
addVarNames vars (Env env) =
    Env (vars :: env)
