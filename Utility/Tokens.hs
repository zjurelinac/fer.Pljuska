module Utility.Tokens (
    isData,
    isParens,
    operatorPrecedence
) where

--import Language.Definitions
import Parsing.Tokenizer


isParens :: Token -> Bool
isParens LeftParens     = True
isParens RightParens    = True
isParens _              = False


isData :: Token -> Bool
isData ( IntToken _ )       = True
isData ( VariableToken _ )  = True
isData ( StringToken _ )    = True
isData _                    = False


operatorPrecedence :: Token -> Int
operatorPrecedence BinaryPlusToken  =   5
operatorPrecedence BinaryMinusToken =   5
operatorPrecedence UnaryMinusToken  =   8
operatorPrecedence MultiplyToken    =   6
operatorPrecedence DivideToken      =   6
operatorPrecedence ModuloToken      =   6
operatorPrecedence LeftParens       =   9
operatorPrecedence RightParens      =   9
