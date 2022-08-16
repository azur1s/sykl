module VM where

import Text.Parsec (SourcePos)
import Data.List (intersperse)

data InstructionType = Op String
                     | Push Value
    deriving (Show)

data Instruction = Instruction
    { code :: InstructionType
    , loc  :: (SourcePos, SourcePos)
    } deriving (Show)

data Value = Int    Int
           | Float  Float
           | Bool   Bool
           | String String
           | List   [Value]
    deriving (Show, Eq)

data VM = VM
    { instrs :: [Instruction]
    , ip     :: Int
    , stack  :: [Value]
    , output :: [String]
    , panic  :: Bool
    } deriving (Show)


fmt :: Value -> String
fmt (Int i)    = show i
fmt (Float f)  = show f
fmt (Bool b)   = show b
fmt (String s) = s
fmt (List l)   = "[" ++ (concat $ intersperse ", " $ map fmt l) ++ "]"

initVM :: [Instruction] -> VM
initVM is = VM is 0 [] [] False

out :: String -> VM -> VM
out msg vm = vm { output = msg : output vm }

err :: String -> VM -> VM
err msg vm = vm { panic = True, output = ("Error `" ++ (show $ code (instrs vm !! ip vm)) ++ "`: " ++ msg) : output vm }

finalize :: VM -> String
finalize vm = unlines $ reverse $ output vm

push :: Value -> VM -> VM
push x vm = vm { stack = x : stack vm }

pop :: VM -> (Maybe Value, VM)
pop vm = case stack vm of
    []     -> (Nothing, vm)
    (x:xs) -> (Just x, vm { stack = xs })

stackEmpty :: VM -> Bool
stackEmpty vm = null $ stack vm