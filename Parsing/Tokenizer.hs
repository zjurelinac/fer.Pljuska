module Parsing.Tokenizer where

import Data.Char
import Data.List
import Data.Ord

import Utility.Data

-- /int-int error - absolute paths should be in quotes
data Token  = StringToken       String      -- Anything between ""
            | IntToken          Int         -- A numeric constant
            | CommentToken      String      -- Anything after # in a line
            | VariableToken     String      -- A variable identifier, can contain alphanumerics and _

        -- Temporary tokens, will be further specified later
            | OperatorToken     String      -- One of the allowed operators:    + - * / %  < > >> = |  == != < > <= >=  || && !
            | IdentifierToken   String      -- Temporary, will be converted into semantic ones
            | SymbolToken       String      -- One of the following: (){}[]
            | NullToken                     -- Indicates a failed match

        -- Semantic tokens
            | ControlToken      String      -- For a control loop or statement
            | CommandToken      String      -- A potential command call
            | ParameterToken    String      -- Parameters of a command

        -- Specific tokens
            -- Arithmetic operators
            | BinaryPlusToken
            | BinaryMinusToken
            | UnaryMinusToken
            | MultiplyToken
            | DivideToken
            | ModuloToken

            -- Comparison operators
            | GreaterToken
            | LesserToken
            | EqualToken
            | NotEqualToken
            | LesserEqualToken
            | GreaterEqualToken

            -- Logical operators
            | NotToken
            | AndToken
            | OrToken

            -- Control tokens
            | AssignToken
            | PipeToken
            | InRedirectToken
            | OutRedirectToken
            | AppendOutRedirectToken

            -- Brackets
            | LeftParens
            | RightParens

            | TestStart
            | TestEnd

            | BlockStart
            | BlockEnd

            deriving ( Show )


type Tokenizer = ( String -> ( Token, String ) )


tokenDelims :: String
tokenDelims = " \t\n()[]{}+-*/%&|"


restoreAll :: ( String, String ) -> ( String, String )
restoreAll ( xs, ys ) = ( "", reverse xs ++ ys )


readComment :: Tokenizer
readComment xs
    | null recognized   = ( NullToken, xs )
    | otherwise         = ( CommentToken . tail $ reverse recognized, left )    -- Clear starting #
    where
        ( recognized, left ) = readComment' ( "", xs )

        readComment' :: ( String, String ) -> ( String, String )
        readComment' a@( ys, [] ) = a                                   -- Input termination
        readComment' a@( [], ( x : xs ) )                               -- Start reading
            | x == '#'      = readComment' ( [ x ], xs )
            | otherwise     = a
        readComment' ( ys, ( x : xs ) ) = readComment' ( x : ys, xs )   -- Propagation if conditions satisfied


readString :: Tokenizer
readString xs
    | null recognized   = ( NullToken, xs )
    | otherwise         = ( StringToken . tail . reverse . tail $ recognized, left )    -- Clear quotes
    where
        ( recognized, left ) = readString' ( "", xs )

        readString' :: ( String, String ) -> ( String, String )
        readString' a@( ys, [] ) = a                                    -- Input termination
        readString' a@( [], ( x : xs ) )                                -- Start reading
            | x == '\"'     = readString' ( [ x ], xs )
            | otherwise     = a
        readString' ( yl@( y : ys ), ( x : xs ) )
            | x == '\"' && y /= '\\'    = ( x : yl, xs )                -- Token ending
            | otherwise                 = readString' ( x : yl, xs )    -- Propagation


readInt :: Tokenizer
readInt xs
    | null recognized   = ( NullToken, xs )
    | otherwise         = ( IntToken . read . reverse $ recognized, left )  -- Convert to integer
    where
        ( recognized, left ) = readInt' ( "", xs )

        readInt' :: ( String, String ) -> ( String, String )
        readInt' a@( ys, [] ) = a                                       -- Input termination
        readInt' a@( [], ( x : xs ) )                                   -- Start reading
            | x == '-' || isDigit x = readInt' ( [ x ], xs )
            | otherwise             = a
        readInt' a@( ys, ( x : xs ) )
            | isDigit x             = readInt' ( x : ys, xs )           -- Propagation
            | x `elem` tokenDelims  = a                                 -- Allowed termination
            | otherwise             = restoreAll a                      -- Matching failure


readVar :: Tokenizer
readVar xs
    | null recognized   = ( NullToken, xs )
    | otherwise         = ( VariableToken . tail . reverse $ recognized, left ) -- Clear starting $
    where
        ( recognized, left ) = readVar' ( "", xs )

        readVar' :: ( String, String ) -> ( String, String )
        readVar' a@( ys, [] ) = a                                       -- Input termination
        readVar' a@( [], ( x : xs ) )                                   -- Start reading
            | x == '$'      = readVar' ( [ x ], xs )
            | otherwise     = a
        readVar' a@( ys, ( x : xs ) )
            | isVarIdentifier x     = readVar' ( x : ys, xs )           -- Propagation
            | otherwise             = a                                 -- Termination


readIdentifier :: Tokenizer
readIdentifier xs
    | null recognized   = ( NullToken, xs )
    | otherwise         = ( IdentifierToken $ reverse recognized, left )
    where
        ( recognized, left ) = readIdentifier' ( "", xs )

        readIdentifier' :: ( String, String ) -> ( String, String )
        readIdentifier' a@( ys, [] ) = a                                -- Input termination
        readIdentifier' a@( [], ( x : xs ) )                            -- Start reading
            | isIdentifierStart x   = readIdentifier' ( [ x ], xs )
            | otherwise             = a
        readIdentifier' a@( yl@( y : ys ), ( x : xs ) )
            | ( x == ' ' && y == '\\' )
                || isIdentifier x   = readIdentifier' ( ( x : yl ), xs )-- Propagation - allow space escaping
            | otherwise             = a                                 -- Termination


readOperator :: Tokenizer
readOperator xs = if null len2ops
    then
        if null len1ops
            then ( NullToken, xs )
            else ( OperatorToken ( head len1ops ), tail xs )
    else
        ( OperatorToken ( head len2ops ), drop 2 xs )

    where
        selectFrom  = ( filter ( `isPrefixOf` xs ) )
        len2ops     = selectFrom [ "==", "<=", ">=", "!=", ">>", "||", "&&" ]
        len1ops     = selectFrom [ "+", "-", "*", "/", "%", "<", ">", "=", "|", "!" ]


readSymbol :: Tokenizer
readSymbol [] = ( NullToken, "" )
readSymbol ( x : xs )
    | x `elem` "()[]{}"     = ( SymbolToken [ x ], xs )
    | otherwise             = ( NullToken, x : xs )


tokenizeString' :: String -> [ Token ]
tokenizeString' [] = []
tokenizeString' xs
    | length rest < length xs   = tok : tokenizeString' rest
    | otherwise                 = error "Syntax error - unrecognized input"
    where
        ( tok, rest )   = minimumBy ( comparing ( length . snd ) ) $ map ( applyOn xs' ) tokenizers
        xs'             = trimBefore xs

        applyOn :: a -> ( a -> b ) -> b
        applyOn ys f    = f ys

        tokenizers :: [ Tokenizer ]
        tokenizers = [
            readComment,
            readString,
            readOperator,
            readInt,
            readVar,
            readSymbol,
            readIdentifier ]


data Context = ArithmeticContext | CommandContext | ConditionContext | NoContext
             deriving ( Eq, Show )

-- Check for minuses and strings
contextualizeTokens :: [ Token ] -> [ Token ]
contextualizeTokens = reverse . fst . foldl determineToken ( [], NoContext )
    where
            determineToken :: ( [ Token ], Context ) -> Token -> ( [ Token ], Context )
            determineToken ( xs, c ) t@( IdentifierToken it )
                | it `elem` commandWords    = ( ControlToken it : xs, NoContext )
                | null xs                   = ( CommandToken it : xs, CommandContext )
                | otherwise                 = case c of
                                                CommandContext  ->  ( ParameterToken it : xs, c )
                                                NoContext       ->  ( CommandToken it : xs, CommandContext )
                                                _               ->  ( ParameterToken ( it ++ show c ) : xs, c )--error "Unknown context"

            determineToken ( xs, c ) t@( OperatorToken ot )
                | ot == "+"     = ( BinaryPlusToken : xs, ArithmeticContext )
                | ot == "*"     = ( MultiplyToken : xs, ArithmeticContext )
                -- bugfix needed
                | ot == "/"     = ( DivideToken : xs, ArithmeticContext )

                | ot == "%"     = ( ModuloToken : xs, ArithmeticContext )
                | ot == "-"     = if ( null xs ||
                                        ( case head xs of
                                            VariableToken _     -> False
                                            IntToken _          -> False
                                            _                   -> True )
                                    ) then  ( UnaryMinusToken : xs, ArithmeticContext )
                                    else    ( BinaryMinusToken : xs, ArithmeticContext )

                | ot == "&&"    = ( AndToken : xs, ConditionContext )
                | ot == "||"    = ( OrToken : xs, ConditionContext )
                | ot == "!"     = ( NotToken : xs, ConditionContext )

                | ot == "="     = ( AssignToken : xs, NoContext )
                | ot == "|"     = ( PipeToken : xs, NoContext )
                | ot == ">>"    = ( AppendOutRedirectToken : xs, CommandContext )


                | ot == "<"     = case c of
                                    ConditionContext    ->  ( LesserToken : xs, c )
                                    _                   ->  ( InRedirectToken : xs, c )
                | ot == ">"     = case c of
                                    ConditionContext    ->  ( GreaterToken : xs, c )
                                    _                   ->  ( OutRedirectToken : xs, c )
                | ot == ">="    = ( GreaterEqualToken : xs, ConditionContext )
                | ot == "<="    = ( LesserEqualToken : xs, ConditionContext )
                | ot == "=="    = ( EqualToken : xs, ConditionContext )
                | ot == "!="    = ( NotEqualToken : xs, ConditionContext )

                | otherwise     = error "Unrecognized operator appeared (Should not happen!)"

            determineToken ( xs, c ) t@( SymbolToken st )
                | st == "("     = ( LeftParens : xs, c )
                | st == ")"     = ( RightParens : xs, c )
                | st == "["     = ( TestStart : xs, ConditionContext )
                | st == "]"     = ( TestEnd : xs, ConditionContext )
                | st == "{"     = ( BlockStart : xs, NoContext )
                | st == "}"     = ( BlockEnd : xs, NoContext )

            determineToken ( xs, c ) t = ( t : xs, c )

            commandWords = [ "if", "while", "else" ]


tokenizeString :: String -> [ Token ]
tokenizeString = contextualizeTokens . tokenizeString'