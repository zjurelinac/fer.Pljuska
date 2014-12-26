module Parsing.Tokenizer where

import Data.Char
import Data.List
import Data.Ord

import Utility.Data


-- Possibly merge minuses with a parameter token
data Token  = StringToken       String      -- Anything between ""
            | IntToken          Int         -- A numeric constant
            | CommentToken      String      -- Anything after # in a line
            | VariableToken     String      -- A variable identifier, can contain alphanumerics and _
            | SymbolToken       String      -- One of the following: (){}[]

        -- Temporary tokens, will be further specified later
            | OperatorToken     String      -- One of the allowed operators:    + - * / %  < > >> = |  == != < > <= >=  || && !
            | IdentifierToken   String      -- Temporary, will be converted into semantic ones
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
            | OutRetirectToken
            | AppendOutRedirectToken

            deriving ( Show )


type Tokenizer = ( String -> ( Token, String ) )


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
            | isDigit x     = readInt' ( [ x ], xs )
            | otherwise     = a
        readInt' a@( ys, ( x : xs ) )
            | isDigit x     = readInt' ( x : ys, xs )                   -- Propagation
            | otherwise     = a                                         -- Termination


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
            readInt,
            readVar,
            readOperator,
            readSymbol,
            readIdentifier ]


contextualizeTokens :: [ Token ] -> [ Token ]
contextualizeTokens = reverse . foldl determineToken []
    where
            determineToken :: [ Token ] -> Token -> [ Token ]
            determineToken xs t@( IdentifierToken it )
                | it `elem` commandWords    = ControlToken it : xs
                | null xs                   = CommandToken it : xs
                | otherwise                 = case ( head xs ) of
                    ( CommandToken _ )      -> ParameterToken it : xs
                    ( ParameterToken _ )    -> ParameterToken it : xs
                    _                       -> CommandToken it : xs

            determineToken xs t@( OperatorToken ot )
                | ot == "+"     = BinaryPlusToken : xs
                | ot == "*"     = MultiplyToken : xs
                | ot == "/"     = DivideToken : xs
                | ot == "%"     = ModuloToken : xs
                | ot == "-"     = if ( null xs ||
                                        ( case head xs of
                                            VariableToken _     -> False
                                            IntToken _          -> False
                                            _                   -> True )
                                    ) then  UnaryMinusToken : xs
                                    else    BinaryMinusToken : xs

                | ot == "&&"    = AndToken : xs
                | ot == "||"    = OrToken : xs
                | ot == "!"     = NotToken : xs

                | ot == "="     = AssignToken : xs
                | ot == "|"     = PipeToken : xs
                | ot == ">>"    = AppendOutRedirectToken : xs

                | ot == "<"     = if isLogicContext xs
                                    then LesserToken : xs
                                    else InRedirectToken : xs
                | ot == ">"     = if isLogicContext xs
                                    then GreaterToken : xs
                                    else OutRetirectToken : xs

                | ot == ">="    = GreaterEqualToken : xs
                | ot == "<="    = LesserEqualToken : xs
                | ot == "=="    = EqualToken : xs
                | ot == "!="    = NotEqualToken : xs

                | otherwise     = error "Unrecognized operator appeared (Should not happen!)"

            determineToken xs t = t : xs

            commandWords = [ "if", "while", "else" ]

            isLogicContext :: [ Token ] -> Bool
            isLogicContext xs
                | null xs       = error "Syntax error: wrong usage of < or > operators"
                | otherwise     = case head xs of
                    ( CommandToken _ )      -> False
                    ( ParameterToken _ )    -> False
                    _                       -> True


tokenizeString :: String -> [ Token ]
tokenizeString = contextualizeTokens . tokenizeString'
