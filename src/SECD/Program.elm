module SECD.Program exposing (..)

import Dict exposing (Dict)
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

        AST.Var (AST.Token variable) ->
            lookup variable env

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


compileFuncApp : Environment -> AST -> List AST -> Result Error (List Op)
compileFuncApp env f args =
    let
        checkArity : ( Maybe Int, List Op, Bool ) -> Result Error ( List Op, Bool )
        checkArity ( mArity, compiledFunction, isBuiltIn ) =
            case mArity of
                Just arity ->
                    if arity >= List.length args then
                        Ok ( compiledFunction, isBuiltIn )

                    else
                        Err <| "Invalid number of arguments"

                Nothing ->
                    Ok ( compiledFunction, isBuiltIn )

        addArguments : ( List Op, Bool ) -> Result Error (List Op)
        addArguments ( compiledFunction, isBuiltIn ) =
            Result.map
                (\compiledArgs ->
                    compiledArgs ++ compiledFunction
                )
                (compileArgs env isBuiltIn args)
    in
    compileFunc env f
        |> Result.andThen checkArity
        |> Result.andThen addArguments



-- Compiles the function that we're calling arguments with
-- Returns : ( Arity, Compiled Function, isBuiltIn )
-- the Arity is Nothing if we don't know how many arguments it takes (e.g. an if statement)


compileFunc : Environment -> AST -> Result Error ( Maybe Int, List Op, Bool )
compileFunc env f =
    case f of
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
        -- rerun compileFunc, but substitute with the value of the token
        -- we get lazy and just say we don't know the value of any let-binding function, but that's ok
        -- I can fix that later, right?
        AST.Var (AST.Token token) ->
            lookup token env
                |> Result.map (\val -> ( Nothing, val, False ))

        AST.Val _ ->
            Err "Illegal function call - attempting to call an integer!"

        AST.Quote _ ->
            Err "Illegal function call - attempting to call a quote value!"

        -- currying
        AST.FuncApp f_ args_ ->
            let
                checkArity : ( Maybe Int, List Op, Bool ) -> Result Error ( Maybe Int, List Op, Bool )
                checkArity ( funcArity, compiledFunc, isBuiltin ) =
                    let
                        newArity =
                            Maybe.map (\n -> n - List.length args_) funcArity
                    in
                    case newArity of
                        Just n ->
                            if n < 0 then
                                Err "CompileFunc - Too many arguments"

                            else
                                Ok ( newArity, compiledFunc, isBuiltin )

                        Nothing ->
                            Ok ( newArity, compiledFunc, isBuiltin )

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

        -- idk how do compile user defined functions yet
        _ ->
            Err "Compile Function - not implemented yet."



-- ifthe arguments are from a builtin function, we can just push them onto the stack
-- otherwise, make a list out of the arguments.


compileArgs : Environment -> Bool -> List AST -> Result Error (List Op)
compileArgs env isBuiltIn args =
    let
        compileNonBuiltin : List AST -> Result Error (List Op)
        compileNonBuiltin args_ =
            case args_ of
                [] ->
                    Ok [ NIL ]

                arg :: xs ->
                    Result.map2
                        (\cmpArg cmpArgs -> FUNC CONS :: (cmpArg ++ cmpArgs))
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

    else if isBuiltIn then
        Result.map List.reverse (compileBuiltin args)

    else
        Result.map List.reverse (compileNonBuiltin args)



---- ENVIRONMENT ----
-- keeps track of the environment, or the value of a variable
-- when we immediately come across a variable name we don't know, we exit, unlike lisp


type alias Environment =
    { -- a simple map of an identifier to its compiled value
      -- Making this a dictoinary means we prevent shadowing, since each let binding identifier must be unique
      letBindings : Dict String (List Op)

    -- doubly nested list of argument names
    -- Used to calculate the "coordinates" of each value
    , functionClosure : List (List String)
    }


emptyEnv : Environment
emptyEnv =
    { letBindings = Dict.empty
    , functionClosure = []
    }



-- Lookup the value of a variable
-- we first look at the function closure, then the let binding closure


lookup : String -> Environment -> Result Error (List Op)
lookup var env =
    case lookupFunctionClosure var env.functionClosure of
        Nothing ->
            case Dict.get var env.letBindings of
                Nothing ->
                    Err <| "Unknown variable " ++ var ++ "!"

                Just val ->
                    Ok val

        Just loadC ->
            Ok [ loadC ]



-- returns an apprioriate LD (x.y), else fails with "unknown variable"


lookupFunctionClosure : String -> List (List String) -> Maybe Op
lookupFunctionClosure var env =
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

        helper : List (List String) -> Int -> Maybe Op
        helper l y =
            case l of
                [] ->
                    Nothing

                hd :: tl ->
                    case searchLine hd y 1 of
                        Just op ->
                            Just op

                        Nothing ->
                            helper tl (y + 1)
    in
    helper env 1



-- Adding Let Bindings, making sure fail when there is a name collision


addLetBindings : List ( String, List Op ) -> Environment -> Result Error Environment
addLetBindings bindings env =
    case bindings of
        [] ->
            Ok env

        ( var, val ) :: binds ->
            case Dict.get var env.letBindings of
                Just _ ->
                    Err <| "Variable '" ++ var ++ "' was already defined above!"

                Nothing ->
                    addLetBindings binds { env | letBindings = Dict.insert var val env.letBindings }
