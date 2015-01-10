module Parsing.Parser(
    parseInput
) where

import Language.Definitions

import Parsing.Tokenizer

import Utility.Data
import Utility.Tokens


type Parser = ( [ Token ] -> ( Expression, [ Token ] ) )


parseArithmetic :: Parser
parseArithmetic toks = ( ArithmeticExpr $ head $ parseArithmetic' [] $ convertToRPN ts, tail rest )
    where
            ( ts, rest ) = break isEnd toks
            parseArithmetic' :: [ Arithmetic ] -> [ Token ] -> [ Arithmetic ]
            parseArithmetic' as [] = as
            parseArithmetic' as ( tok : ts ) = parseArithmetic'
                ( case tok of
                    IntToken it         -> ( Value . StaticData . IntValue $ it ) : as
                    StringToken st      -> ( Value . StaticData . StringValue $ st ) : as
                    VariableToken vt    -> ( Value . VarData . Variable $ vt ) : as
                    UnaryMinusToken     -> ( UnaryArithmetic UnaryMinus ( head as ) ) : ( tail as )
                    BinaryPlusToken     -> ( Arithmetic Plus ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    BinaryMinusToken    -> ( Arithmetic Minus ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    MultiplyToken       -> ( Arithmetic Multiply ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    DivideToken         -> ( Arithmetic Divide ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    ModuloToken         -> ( Arithmetic Modulo ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    t                   -> error $ "Unrecognized token: " ++ show t
                ) ts


-- Potential error, check reversing operators -- EXTENSIVE TESTING REQUIRED
convertToRPN :: [ Token ] -> [ Token ]
convertToRPN = reverse . convertToRPN' [] []
    where
            convertToRPN' :: [ Token ] -> [ Token ] -> [ Token ] -> [ Token ]
            convertToRPN' out [] []  = out
            convertToRPN' out a@( op : ops ) []
                | isParens op   = error "Mismatched parentheses"
                | otherwise     = convertToRPN' ( op : out ) ops []
            convertToRPN' out ops ( y : ys )
                | isData y      = convertToRPN' ( y : out ) ops ys
                | otherwise     = case y of
                    UnaryMinusToken     -> convertToRPN' out ( y : ops ) ys
                    NotToken            -> convertToRPN' out ( y : ops ) ys
                    LeftParens          -> convertToRPN' out ( y : ops ) ys
                    RightParens         ->  let ops'    = takeWhile ( not . isLeftParens ) ops
                                                ns      = drop ( length ops' + 1 ) ops      -- could make an exception, if parentheses mismatched
                                                func    = if null ns || ( not . isUnaryMinus $ head ns )
                                                          then []
                                                          else [ head ns ]
                                                ns'     = if null func then ns else tail ns
                                            in convertToRPN' ( func ++ reverse ops' ++ out ) ns' ys
                    _                   -> let  pr      = operatorPrecedence y
                                                ops'    = takeWhile ( shouldPop pr ) ops
                                                ns      = drop ( length ops' ) ops
                                           in convertToRPN' ( reverse ops' ++ out ) ( y : ns ) ys

            shouldPop :: Int -> Token -> Bool
            shouldPop i x
                | isData x || isLeftParens x    = False
                | otherwise                     = operatorPrecedence x >= i


parseCommand :: Parser
parseCommand toks = ( CommandExpr $ parseCommand' False ts, tail rest )
    where
            ( ts, rest ) = break isEnd toks


parseCommand' :: Bool -> [ Token ] -> Command
parseCommand' inpipe xs
    | null rest     = Basic $ parseBasic toks inpipe False
    | otherwise     = PipedCommand ( parseBasic toks inpipe True ) ( parseCommand' True $ tail rest )
    where   ( toks, rest ) = break isPipeToken xs

            parseBasic :: [ Token ] -> Bool -> Bool -> BasicCommand
            parseBasic toks inpipe outpipe = BasicCommand {
                cmdName         = getCmdName . head $ toks,
                args            = parseArgs . tail $ toks,
                inputStream     = instream,
                outputStream    = outstream,
                append          = shouldAppend,
                displayOutput   = not outpipe,
                pipedInto       = inpipe
            }

                where
                        instream        = if length ins > 1 then Just ( convertToData $ ins !! 1 ) else Nothing
                        outstream       = if length outs > 1 then Just ( convertToData $ outs !! 1 ) else Nothing
                        shouldAppend    = if null outs then False else isAppend $ head outs
                        parseArgs       = map convertToData . takeWhile isDataOrParam
                        outs            = dropWhile ( not . isOutRedirect ) toks
                        ins             = dropWhile ( not . isInRedirect ) toks


hideCommandOutput :: Expression -> Expression
hideCommandOutput ( CommandExpr cmd )   = CommandExpr $ hideCommandOutput' cmd
hideCommandOutput _                     = error "Not a command, cannot hide output."


hideCommandOutput' :: Command -> Command
hideCommandOutput' ( Basic bc ) = Basic $ bc { displayOutput = False }
hideCommandOutput' ( PipedCommand bc c ) = PipedCommand bc $ hideCommandOutput' c


data CondBuilder = BasicCond Condition | Combinator Token
                 deriving ( Show )


preprocessCond :: [ Token ] -> [ Token ] -> [ CondBuilder ]
preprocessCond xs []
    | null xs           = []
    | otherwise         = error "Syntax error in condition"
preprocessCond xs ( y : ys )
    | isData y          = preprocessCond ( y : xs ) ys
    | isComparator y    = BasicCond ( BasicCondition ( toComparison y ) ( convertToData $ xs !! 1 ) ( convertToData $ xs !! 0 ) )
                            : preprocessCond [] ys
    | otherwise         = ( Combinator y ) : preprocessCond [] ys


parseCondition' :: [ CondBuilder ] -> Condition
parseCondition' = parseCondition'' []
    where
            parseCondition'' :: [ Condition ] -> [ CondBuilder ] -> Condition
            parseCondition'' xs [] = head xs
            parseCondition'' xs ( y : ys ) = case y of
                ( BasicCond bc )        ->  parseCondition'' ( bc : xs ) ys
                ( Combinator NotToken ) ->  parseCondition'' ( NotCondition ( head xs ) : tail xs ) ys
                ( Combinator OrToken )  ->  parseCondition'' ( OrCondition ( xs !! 1 ) ( xs !! 0 ) : drop 2 xs ) ys
                ( Combinator AndToken ) ->  parseCondition'' ( AndCondition ( xs !! 1 ) ( xs !! 0 ) : drop 2 xs ) ys


parseCondition :: ( [ Token ] -> ( Condition, [ Token ] ) )
parseCondition ( TestStart : toks ) = ( parseCondition' . preprocessCond [] . convertToRPN $ ts, tail rest )
    where ( ts, rest ) = break isTestEnd toks
parseCondition _    = error "Not a condition"


parseAssignment :: Parser
parseAssignment ( VariableToken v : AssignToken : toks )    = ( AssignmentExpr $ Assignment ( Variable v ) expr, tail rest )
    where
            ( ts, rest )    = break isEnd toks
            expr            = case head ts of
                ( CommandToken _ )  ->  hideCommandOutput . fst . parseCommand $ ts
                _                   ->  fst . parseArithmetic $ ts
parseAssignment _                                           = error "Syntax error: invalid assignment"


parseExpression :: Parser
parseExpression toks = case head toks of
    ( VariableToken _ )     -> parseAssignment toks
    ( CommandToken _ )      -> parseCommand toks
    ( ControlToken v )      -> case v of
                        "if"    ->  parseIfBlock toks
                        "while" ->  parseWhileBlock toks
    EndToken                -> ( VoidExpr, tail toks )
    _                       -> error "Invalid expression"



parseIfBlock :: Parser
parseIfBlock ( ControlToken ct : toks )
    | ct == "if"    = ( BlockExpr $ IfBlock cond b1 b2, rest2 )
    | otherwise     = error "Not an if block"
        where
                ( cond, rest )  = parseCondition toks
                ( b1, rest1 )   = parseBasicBlock rest
                ( b2, rest2 )   = if ( not $ null rest1 ) && head rest1 == ( ControlToken "else" )
                                then parseBasicBlock ( tail rest1 )
                                else ( BlockExpr $ BasicBlock [ VoidExpr ], rest1 )
parseIfBlock _      = error "Not an if block"

parseWhileBlock :: Parser
parseWhileBlock ( ControlToken ct : toks )
    | ct == "while"     = ( BlockExpr $ WhileBlock cond b, rest1 )
    | otherwise         = error "Not a while block"
        where
                ( cond, rest )  = parseCondition toks
                ( b, rest1 )    = parseBasicBlock rest
parseWhileBlock _       = error "Not a while block"


parseBasicBlock :: Parser
parseBasicBlock ( BlockStart : toks ) = ( BlockExpr $ BasicBlock exprs, rest' )
    where
            ( exprs, rest ) = parseBBlock toks
            rest'           = if null rest || head rest /= EndToken then rest else tail rest
parseBasicBlock _ = error "Not a block"


parseBBlock :: [ Token ] -> ( [ Expression ], [ Token ] )
parseBBlock ( BlockEnd : ts ) = ( [], ts )
parseBBlock ts = ( expr : next, rest' )
    where
            ( expr, rest )  = parseExpression ts
            ( next, rest' ) = parseBBlock rest


parseInput :: String -> Expression
parseInput = fst . parseBasicBlock . tokenizeInput
