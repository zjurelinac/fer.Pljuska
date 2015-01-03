module Utility.Tokens (
    convertToData,
    getCmdName,
    isAppend,
    isData,
    isDataOrParam,
    isInRedirect,
    isLeftParens,
    isOutRedirect,
    isParens,
    isPipeToken,
    isUnaryMinus,
    operatorPrecedence
) where


import Language.Definitions
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


isDataOrParam :: Token -> Bool
isDataOrParam ( ParameterToken _ )  = True
isDataOrParam x                     = isData x


isLeftParens :: Token -> Bool
isLeftParens LeftParens = True
isLeftParens _          = False


isPipeToken :: Token -> Bool
isPipeToken PipeToken   = True
isPipeToken _           = False


isUnaryMinus :: Token -> Bool
isUnaryMinus UnaryMinusToken    = True
isUnaryMinus _                  = False


isInRedirect :: Token -> Bool
isInRedirect InRedirectToken    = True
isInRedirect _                  = False


isOutRedirect :: Token -> Bool
isOutRedirect OutRedirectToken          = True
isOutRedirect AppendOutRedirectToken    = True
isOutRedirect _                         = False


isAppend :: Token -> Bool
isAppend AppendOutRedirectToken     =   True
isAppend _                          =   False

operatorPrecedence :: Token -> Int
operatorPrecedence BinaryPlusToken  =   5
operatorPrecedence BinaryMinusToken =   5
operatorPrecedence UnaryMinusToken  =   8
operatorPrecedence MultiplyToken    =   6
operatorPrecedence DivideToken      =   6
operatorPrecedence ModuloToken      =   6
operatorPrecedence LeftParens       =   9
operatorPrecedence RightParens      =   9


convertToData :: Token -> Data
convertToData ( IntToken i )        = StaticData $ IntValue i
convertToData ( StringToken s )     = StaticData $ StringValue s
convertToData ( ParameterToken p )  = StaticData $ StringValue p
convertToData ( VariableToken v )   = VarData $ Variable v
convertToData _                     = error "Cannot convert to data object"


getCmdName :: Token -> String
getCmdName ( CommandToken c )   = c
getCmdName _                    = error "Not a command"
