module Language.Definitions where

import qualified Data.Map as M

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
                        variables           :: VarTable,
                        executionStatus     :: Status
                    } deriving ( Show )

-- Language values
data Data           = VarData Variable
                    | StaticData PrimitiveType
                    deriving ( Show )

-- Current environment status
data Status         = StatusOK
                    | StatusError Error
                    deriving ( Show )

-- Possible errors
data Error          = IOError String          -- Some IO operation went wrong
                    | CallError String        -- Wrong command call
                    | ArithmeticError String  -- Impossible arithmetic operation, like division by zero
                    | TypeError String        -- Operator or an assignment got the wrong type
                    | UnknownError String     -- Everything else
                    deriving ( Show )

-- Data type of each command function
type CommandFunction    = ( [ PrimitiveType ] -> IO ( Either Error PrimitiveType ) )

-- A basic command definition
data BasicCommand   = BasicCommand {
                        cmdName         :: String,
                        args            :: [ Data ],
                        environment     :: Environment,
                        inputStream     :: Maybe Data,
                        outputStream    :: Maybe Data,
                        append          :: Bool
                    } deriving ( Show )

-- Command definition that allows chaining
data Command        = Basic BasicCommand
                    | PipedCommand BasicCommand Command
                    deriving ( Show )

-- A basic condition, could one day be extended to allow complex tests
data Condition      = Condition {
                        operator :: Comparison,
                        operand1 :: Data,
                        operand2 :: Data
                    } deriving ( Show )

-- Definition of a logic operator for condition testing
data Comparison     = Equals | NotEqual | Greater | Lesser | GreaterEqual | LesserEqual
                    deriving ( Show )

-- Definition of a general binary operator
data Operator       = Plus | Minus | Multiply | Divide | Modulo | Power | Concat
                    deriving ( Show )

-- Definition of an expression
data Expression     = Assignment Variable Expression                -- Assigns the result of an expression to the variable
                    | OperatorExpr Operator Expression Expression   -- Returns the result of an operation on the operands
                    | ConditionExpr Condition                       -- Returns Bool as a result of evaluating a condition
                    | CommandExpr Command                           -- Returns the result of a command execution
                    | DataExpr Data                                 -- Returns the value of a given data element
                    | VoidExpr                                      -- Does precisely nothing -> useful in IF without the ELSE
                    deriving ( Show )

-- Definition of a execution block
data Block          = BasicBlock [ Expression ]                             -- A thread of commands meant to be executed sequentially
                    | IfBlock Expression Block Block                        -- If block, depending on the condition, execute either the first or the second block
                    | ForBlock Expression Expression Expression Block       -- For block, containing initial assignment, test condition, increment and expression block
                    | WhileBlock Expression Block                           -- Execute block while condition evaluates to True
                    deriving ( Show )
