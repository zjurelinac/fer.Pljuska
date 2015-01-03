module Parsing.Parser where

import Language.Definitions

import Parsing.Tokenizer

import Utility.Data
import Utility.Tokens



parseArithmetic :: [ Token ] -> Expression
parseArithmetic = ArithmeticExpr . head . parseArithmetic' []
    where
            parseArithmetic' :: [ Arithmetic ] -> [ Token ] -> [ Arithmetic ]
            parseArithmetic' as [] = as
            parseArithmetic' as ( tok : ts ) = parseArithmetic'
                ( case tok of
                    IntToken it         -> ( Value . StaticData . IntValue $ it ) : as
                    VariableToken vt    -> ( Value . VarData . Variable $ vt ) : as
                    UnaryMinusToken     -> ( UnaryArithmetic UnaryMinus ( head as ) ) : ( tail as )
                    BinaryPlusToken     -> ( Arithmetic Plus ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    BinaryMinusToken    -> ( Arithmetic Minus ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    MultiplyToken       -> ( Arithmetic Multiply ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    DivideToken         -> ( Arithmetic Divide ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    ModuloToken         -> ( Arithmetic Modulo ( as !! 1 ) ( as !! 0 ) ) : ( drop 2 as )
                    _                   -> error "Unrecognized token"
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



parseCommand :: Bool -> [ Token ] -> Command
parseCommand inpipe xs
    | null rest     = Basic $ parseBasic toks inpipe False
    | otherwise     = PipedCommand ( parseBasic toks inpipe True ) ( parseCommand True $ tail rest )
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


--parseCondition :: [ Token ] -> Condition
