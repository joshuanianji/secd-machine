module SECD.Program exposing (..)

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
    | ATOM -- true if the element is an atom (integer or boolean)
    | CONS
    | COMPARE Cmp


type Cmp
    = EQ
    | NE
    | LT
    | GT
    | LEQ
    | GEQ



-- turns a Compare to an Elm function that we can use.


cmpFunc : Cmp -> (comparable -> comparable -> Bool)
cmpFunc cmp =
    case cmp of
        EQ ->
            (==)

        NE ->
            (/=)

        LT ->
            (<)

        GT ->
            (>)

        LEQ ->
            (<=)

        GEQ ->
            (>=)


cmpToString : Cmp -> String
cmpToString cmp =
    case cmp of
        EQ ->
            "=="

        NE ->
            "!="

        LT ->
            "<"

        GT ->
            ">"

        LEQ ->
            "<="

        GEQ ->
            ">="



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
