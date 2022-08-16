module Parser where

import qualified Text.Parsec        as P
import qualified Text.Parsec.String as P
import VM

instruction :: String
instruction = "!$%&'()*+,-./:;<=>?" -- 33 to 47 (skipped 34 ", 35 # and 64 @)
        ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" -- 65 to 90
        ++ "\\^_`" -- 91 to 96 (skipped 91 [ and 93 ])
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
    return $ Instruction (Op $ [c]) (start, end)

extended :: P.Parser Instruction
extended = do
    start <- P.getPosition
    _ <- P.char '@'
    c <- P.oneOf instruction
    end <- P.getPosition
    return $ Instruction (Op $ "@" ++ [c]) (start, end)

int :: P.Parser Instruction
int = do
    start <- P.getPosition
    c <- P.many1 P.digit
    end <- P.getPosition
    return $ Instruction (Push $ Int $ read c) (start, end)

float :: P.Parser Instruction
float = do
    start <- P.getPosition
    c <- P.many1 P.digit
    _ <- P.char '.'
    d <- P.many1 P.digit
    end <- P.getPosition
    return $ Instruction (Push $ Float $ read (c ++ "." ++ d)) (start, end)

string :: P.Parser Instruction
string = do
    start <- P.getPosition
    c <- P.between (P.char '"') (P.char '"') (P.many $ P.noneOf "\"" P.<|> P.try (P.string "\"\"" >> return '"'))
    end <- P.getPosition
    return $ Instruction (Push $ String $ c) (start, end)

list :: P.Parser Instruction
list = do
    start <- P.getPosition
    _ <- P.char '[' >> ws
    is <- P.sepBy (P.choice [single, extended, int, float, string]) (ws >> P.char ',' >> ws)
    _ <- P.char ']' >> ws
    end <- P.getPosition
    let c = map (\(Instruction i _) -> (\(Push i') -> i') i) is
    return $ Instruction (Push $ List c) (start, end)

program :: P.Parser [Instruction]
program = P.many (ws *> (single P.<|> extended P.<|> int P.<|> string P.<|> list) <* ws)

parse :: String -> Either P.ParseError [Instruction]
parse s = P.runParser program () "" s