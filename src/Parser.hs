module Parser where

import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P
import VM

instruction :: String
instruction = "!$%&'()*+,-.\\/:;<=>?" -- 33 to 47 (skipped 34 ", 35 # and 64 @)
        ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" -- 65 to 90
        ++ "[]^_`" -- 91 to 96
        ++ "abcdefghijklmnopqrstuvwxyz" -- 97 to 122
        ++ "{|}~" -- 123 to 126

-- | Parsers

ws :: P.Parser ()
ws = P.skipMany (P.oneOf " \t\r\n")

single :: P.Parser Instruction
single = do
    start <- P.getPosition
    c <- P.oneOf instruction
    end <- P.getPosition
    return $ Instruction [c] (start, end)

extended :: P.Parser Instruction
extended = do
    start <- P.getPosition
    _ <- P.char '@'
    c <- P.oneOf instruction
    end <- P.getPosition
    return $ Instruction ("@" ++ [c]) (start, end)

int :: P.Parser Instruction
int = do
    start <- P.getPosition
    c <- P.many1 P.digit
    end <- P.getPosition
    return $ Instruction (c) (start, end)

program :: P.Parser [Instruction]
program = P.many (ws *> (single P.<|> extended P.<|> int) <* ws)

parse :: String -> Either P.ParseError [Instruction]
parse s = P.runParser program () "" s