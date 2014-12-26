module Language.Definitions where

import qualified Data.Map as M


-- Definition of an evaluable typeclass - returns a single value only, doesn't change the environment state
class Evaluable a where
    evaluate :: Environment -> a -> PrimitiveType


-- Definition of an executable typeclass - returns both a value and a changed environment
class Executable a where
    execute :: Environment -> a -> IO ( PrimitiveType, Environment )


-- Definition of a testable typeclass - evaluates to a boolean value
class Testable a where
    test :: Environment -> a -> Bool


-- Value containers, supporting integers and strings
data PrimitiveType  = IntValue Int
                    | StringValue String
                    | NoValue
                    deriving ( Show )


-- A definition of variable
data Variable       = Variable String
                    deriving ( Show )


-- A table of variables and their values
type VarTable       = M.Map String PrimitiveType


-- A definition of a list of available commands
type CommandList    = M.Map String CommandFunction


-- Script execution environment
data Environment    = Environment {
                        commandList         :: CommandList,
                        currentDirectory    :: FilePath,
                        lastReturn          :: PrimitiveType,
                        variables           :: VarTable
                    }

instance Show Environment where
    show env = "Environment\ncd := " ++ ( currentDirectory env )


-- Language values
data Data           = VarData Variable
                    | StaticData PrimitiveType
                    deriving ( Show )


-- Data type of each command function
type CommandFunction    = ( Environment -> [ PrimitiveType ] -> IO ( PrimitiveType, Environment ) )



-- A basic command definition
data BasicCommand   = BasicCommand {
                        cmdName         :: String,
                        args            :: [ Data ],
                        inputStream     :: Maybe Data,
                        outputStream    :: Maybe Data,
                        append          :: Bool,
                        displayOutput   :: Bool,
                        pipedInto       :: Bool
                    } deriving ( Show )


-- Command definition that allows chaining
data Command        = Basic BasicCommand
                    | PipedCommand BasicCommand Command
                    deriving ( Show )


-- Definition of a test condition
data Condition      = BasicCondition Comparison Data Data   -- Compare two variables
                    | AndCondition Condition Condition      -- And operator on two subconditions
                    | OrCondition Condition Condition       -- Or operator on two subconditions
                    | NotCondition Condition                -- Negation of a condition
                    deriving ( Show )


-- Definition of a logic operator for condition testing
data Comparison     = Equals | NotEqual | Greater | Lesser | GreaterEqual | LesserEqual
                    deriving ( Show )


-- Definition of a general binary operator
data Operator       = Plus | Minus | Multiply | Divide | Modulo | Power | UnaryMinus
                    deriving ( Show )


-- Definition of an assignment
data Assignment     = Assignment Variable Expression
                    deriving ( Show )


-- Definition of an arithmetic expression
data Arithmetic     = Arithmetic Operator Arithmetic Arithmetic
                    | UnaryArithmetic Operator Arithmetic
                    | Value Data
                    deriving ( Show )


-- Definition of an expression
data Expression     = AssignmentExpr    Assignment      -- Assigns the result of an expression to the variable
                    | BlockExpr         Block           -- A block of other expressions, default return value
                    | ArithmeticExpr    Arithmetic      -- Returns the result of an operation on the operands
                    | CommandExpr       Command         -- Returns the result of a command execution
                    | DataExpr          Data            -- Returns the value of a given data element
                    | VoidExpr                          -- Does precisely nothing -> useful in IF without the ELSE
                    deriving ( Show )


-- Definition of a execution block
data Block          = BasicBlock [ Expression ]             -- A thread of commands meant to be executed sequentially
                    | IfBlock Condition Block Block         -- If block, depending on the condition, execute either the first or the second block
                    | WhileBlock Condition Block            -- Execute block while condition evaluates to True
                    deriving ( Show )
