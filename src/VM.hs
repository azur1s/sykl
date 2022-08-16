module VM where

import Data.List (intersperse)

data Value = Int    Int
           | Bool   Bool
           | String String
           | List   [Value]
    deriving (Show, Eq)

data VM = VM
    { instrs :: [String]
    , ip     :: Int
    , stack  :: [Value]
    , output :: [String]
    , panic  :: Bool
    } deriving (Show)

fmt :: Value -> String
fmt (Int i)    = show i
fmt (Bool b)   = show b
fmt (String s) = s
fmt (List l)   = "[" ++ (concat $ intersperse ", " $ map fmt l) ++ "]"

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