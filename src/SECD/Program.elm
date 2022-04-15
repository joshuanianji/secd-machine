module SECD.Program exposing (..)

-- | SECD Code


type Program
    = Program (List Op)


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


-- consumable by the SECD VM

toList : Program -> List Op
toList (Program ops) = ops


fromList : List Op -> Program
fromList = Program 


-- useful in testing

fromSingleton : Op -> Program
fromSingleton op = Program [op]