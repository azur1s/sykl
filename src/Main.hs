module Main where

import Text.Read (readMaybe)
import Data.List (intersperse)

data Value = Int    Int
           | Bool   Bool
           | String String
           | List   [Value]
    deriving (Show, Eq)

fmt :: Value -> String
fmt (Int i)    = show i
fmt (Bool b)   = show b
fmt (String s) = s
fmt (List l)   = "[" ++ (concat $ intersperse ", " $ map fmt l) ++ "]"

data VM = VM
    { instrs :: [String]
    , ip     :: Int
    , stack  :: [Value]
    , output :: [String]
    , panic  :: Bool
    } deriving (Show)

initVM :: [String] -> VM
initVM is = VM is 0 [] [] False

out :: String -> VM -> VM
out msg vm = vm { output = msg : output vm }

err :: String -> VM -> VM
err msg vm = vm { panic = True, output = ("Error: " ++ msg ++ " on " ++ (instrs vm !! ip vm)): output vm }

finalize :: VM -> String
finalize vm = unlines $ reverse $ output vm

push :: Value -> VM -> VM
push x vm = vm { stack = x : stack vm }

pop :: VM -> (Maybe Value, VM)
pop vm = case stack vm of
    []     -> (Nothing, vm)
    (x:xs) -> (Just x, vm { stack = xs })

apply :: (Value -> Either VM Value) -> VM -> VM
apply f vm = case pop vm of
    (Just x, vm') -> case f x of
        Left vm'' -> vm''
        Right z   -> push z vm'
    _ -> err "Stack underflow" vm

apply2 :: (Value -> Value -> Either VM Value) -> VM -> VM
apply2 f vm = case pop vm of
    (Just x, vm') -> case pop vm' of
        (Just y, vm'') -> case f x y of
            Left vm''' -> vm'''
            Right z    -> push z vm''
        _ -> err "Stack underflow" vm'
    _ -> err "Stack underflow" vm

add :: VM -> VM
add vm = apply2 (\x y -> case (x, y) of
    (Int i, Int j) -> Right $ Int (i + j)
    _              -> Left $ err "Type error" vm) vm

putv :: VM -> VM
putv vm = apply (\x -> Left $ out (fmt x) vm) vm

step :: VM -> VM
step vm = do
    let instr = instrs vm !! ip vm
    let vm' = case instr of
            "#" -> putv vm
            "+" -> add vm
            _   -> case readMaybe instr :: Maybe Int of
                Just n  -> push (Int n) vm
                Nothing -> err ("Unknown instruction: " ++ instr) vm
    vm' { ip = ip vm' + 1 }

exec :: VM -> VM
exec vm = if panic vm
    then vm
    else if ip vm >= length (instrs vm)
        then vm
        else exec (step vm)

main :: IO ()
main = do
    let vm = initVM ["34", "35", "+", "#"]
    let vm' = exec vm
    putStr $ finalize vm'