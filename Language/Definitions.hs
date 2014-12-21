module Language.Definitions where

import qualified Data.Map as M


-- Definition of an evaluable typeclass
class Evaluable a where
    evaluate :: Environment -> a -> PrimitiveType


-- Definition of an executable typeclass
class Executable a where
    execute :: Environment -> a -> ( PrimitiveType, Environment )


-- Definition of a testable typeclass
class Testable a where
    test :: Environment -> a -> Bool


-- Value containers, supporting integers and strings
data PrimitiveType  = IntValue Int
                    | StringValue String
                    deriving ( Show )


-- A definition of variable
data Variable       = Variable String
                    deriving ( Show )


-- A table of variables and their values
type VarTable       = M.Map String PrimitiveType


-- Script execution environment
data Environment    = Environment {
                        currentDirectory    :: FilePath,
                        variables           :: VarTable
                    } deriving ( Show )


-- Language values
data Data           = VarData Variable
                    | StaticData PrimitiveType
                    deriving ( Show )


defaultReturn :: PrimitiveType
defaultReturn = IntValue 0


-- Data type of each command function
type CommandFunction    = ( Environment -> [ PrimitiveType ] -> IO ( PrimitiveType, Environment ) )


-- A basic command definition
data BasicCommand   = BasicCommand {
                        cmdName         :: String,
                        args            :: [ Data ],
                        environment     :: Environment,
                        inputStream     :: Maybe Data,
                        outputStream    :: Maybe Data,
                        append          :: Bool,
                        displayOutput   :: Bool
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
data Operator       = Plus | Minus | Multiply | Divide | Modulo | Power | Concat
                    deriving ( Show )


-- Definition of an assignment
data Assignment     = Assignment Variable Expression
                    deriving ( Show )


-- Definition of an arithmetic expression
data Arithmetic     = Arithmetic Operator Arithmetic Arithmetic
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
