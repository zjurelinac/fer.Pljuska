module Utility.Tokens (
    convertToData,
    getCmdName,
    isAppend,
    isComparator,
    isData,
    isDataOrParam,
    isEnd,
    isInRedirect,
    isLeftParens,
    isOutRedirect,
    isParens,
    isPipeToken,
    isTestEnd,
    isUnaryMinus,
    operatorPrecedence,
    toComparison
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


isEnd :: Token -> Bool
isEnd EndToken  = True
isEnd _         = False


isTestEnd :: Token -> Bool
isTestEnd TestEnd   = True
isTestEnd _         = False


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
isAppend AppendOutRedirectToken     = True
isAppend _                          = False


isComparator :: Token -> Bool
isComparator GreaterToken           = True
isComparator LesserToken            = True
isComparator EqualToken             = True
isComparator NotEqualToken          = True
isComparator GreaterEqualToken      = True
isComparator LesserEqualToken       = True
isComparator _                      = False


isRedirect :: Token -> Bool
isRedirect x = isOutRedirect x || isInRedirect x


operatorPrecedence :: Token -> Int
operatorPrecedence BinaryPlusToken      =   5
operatorPrecedence BinaryMinusToken     =   5
operatorPrecedence UnaryMinusToken      =   8
operatorPrecedence MultiplyToken        =   6
operatorPrecedence DivideToken          =   6
operatorPrecedence ModuloToken          =   6

operatorPrecedence LeftParens           =   9
operatorPrecedence RightParens          =   9

operatorPrecedence EqualToken           =   10
operatorPrecedence NotEqualToken        =   10
operatorPrecedence GreaterToken         =   10
operatorPrecedence LesserToken          =   10
operatorPrecedence LesserEqualToken     =   10
operatorPrecedence GreaterEqualToken    =   10

operatorPrecedence NotToken             =   8
operatorPrecedence AndToken             =   6
operatorPrecedence OrToken              =   5

operatorPrecedence x                    =   error ( "Unknown operator " ++ show x )


convertToData :: Token -> Data
convertToData ( IntToken i )        = StaticData $ IntValue i
convertToData ( StringToken s )     = StaticData $ StringValue s
convertToData ( ParameterToken p )  = StaticData $ StringValue p
convertToData ( VariableToken v )   = VarData $ Variable v
convertToData x                     = error $ "Cannot convert to data object: " ++ show x


toComparison :: Token -> Comparison
toComparison EqualToken         = Equals
toComparison NotEqualToken      = NotEqual
toComparison LesserToken        = Lesser
toComparison GreaterToken       = Greater
toComparison LesserEqualToken   = LesserEqual
toComparison GreaterEqualToken  = GreaterEqual
toComparison x                  = error $ "Not a comparison token: " ++ show x


getCmdName :: Token -> String
getCmdName ( CommandToken c )   = c
getCmdName x                    = error $ "Not a command: " ++ show x
