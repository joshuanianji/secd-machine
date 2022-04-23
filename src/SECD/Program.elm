module SECD.Program exposing (..)

import Html exposing (Html)
import Html.Attributes as Attr
import Lib.Cons as Cons exposing (Cons)
import Lib.LispAST as AST exposing (AST)
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
    Result.map Program <| compile_ ast


compile_ : AST -> Result Error (List Op)
compile_ ast =
    case ast of
        -- nil vals
        AST.Var (AST.Token "nil") ->
            Ok [ NIL ]

        -- Compile list (nil on empty list)
        AST.Quote cons ->
            Ok <| compileCons cons

        AST.Val n ->
            Ok <| [ LDC n ]

        AST.FuncApp f args ->
            compileFuncApp f args

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


compileFuncApp : AST -> List AST -> Result Error (List Op)
compileFuncApp f args =
    let
        ( argNum, compiledFuncResult ) =
            compileFunc f
    in
    if argNum == Just (List.length args) || argNum == Nothing then
        Result.map2
            (\compiledFunc compiledArgs ->
                compiledArgs ++ LDF :: compiledFunc
            )
            compiledFuncResult
            (compileArgs args)

    else
        Err <| "Invalid number of arguments"



-- compiles the function, also returns the number of arguments it takes
-- the arguments are Nothing if we don't know how many arguments it takes (e.g. a Let binding)


compileFunc : AST -> ( Maybe Int, Result Error (List Op) )
compileFunc f =
    case f of
        AST.Var (AST.Token "+") ->
            ( Just 2, Ok <| [ FUNC ADD ] )

        AST.Var (AST.Token "*") ->
            ( Just 2, Ok <| [ FUNC MULT ] )

        AST.Var (AST.Token "-") ->
            ( Just 2, Ok <| [ FUNC SUB ] )

        AST.Var (AST.Token "atom") ->
            ( Just 1, Ok <| [ FUNC ATOM ] )

        AST.Var (AST.Token "cons") ->
            ( Just 2, Ok <| [ FUNC CONS ] )

        AST.Var (AST.Token "car") ->
            ( Just 1, Ok <| [ FUNC CAR ] )

        AST.Var (AST.Token "cdr") ->
            ( Just 1, Ok <| [ FUNC CDR ] )

        AST.Var (AST.Token "null") ->
            ( Just 1, Ok <| [ FUNC NULL ] )

        AST.Var (AST.Token "eq") ->
            ( Just 2, Ok <| [ FUNC (COMPARE CMP_EQ) ] )

        AST.Var (AST.Token "<") ->
            ( Just 2, Ok <| [ FUNC (COMPARE CMP_LT) ] )

        AST.Var (AST.Token ">") ->
            ( Just 2, Ok <| [ FUNC (COMPARE CMP_GT) ] )

        AST.Var (AST.Token "<=") ->
            ( Just 2, Ok <| [ FUNC (COMPARE CMP_LEQ) ] )

        AST.Var (AST.Token ">=") ->
            ( Just 2, Ok <| [ FUNC (COMPARE CMP_GEQ) ] )

        AST.Var (AST.Token token) ->
            ( Nothing, Err <| "CompileFunc - custom function names not implemented yet! (token: " ++ token ++ ")" )

        AST.Val _ ->
            ( Nothing, Err <| "Illegal function call - attempting to call an integer!" )

        AST.Quote _ ->
            ( Nothing, Err <| "Illegal function call - attempting to call a quote value!" )

        -- idk how do compile user defined functions yet
        _ ->
            ( Nothing, Err "Compile Function - not implemented yet." )


compileArgs : List AST -> Result Error (List Op)
compileArgs args =
    let
        helper : List AST -> Result Error (List Op)
        helper args_ =
            case args_ of
                [] ->
                    Ok [ NIL ]

                arg :: xs ->
                    Result.map2
                        (\cmpArg cmpArgs ->
                            FUNC CONS :: (cmpArg ++ cmpArgs)
                        )
                        (compile_ arg)
                        (helper xs)
    in
    if List.length args == 0 then
        Ok []

    else
        Result.map List.reverse (helper args)
