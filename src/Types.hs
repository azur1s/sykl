module Types where

import Data.List (intersperse)

data Value = Int    Int
           | Bool   Bool
           | String String
           | List   [Value]
           | Nil
    deriving (Show, Eq)

fmt :: Value -> String
fmt (Int i)    = show i
fmt (Bool b)   = show b
fmt (String s) = s
fmt (List l)   = "(" ++ (concat $ intersperse ", " $ map fmt l) ++ ")"
fmt Nil        = "nil"

data State = State
    { stack  :: [Value]
    , ops    :: [String]
    , ptr    :: Int
    , output :: [String]
    , exit   :: Bool
    }
    deriving (Show, Eq)

initWith :: [String] -> State
initWith p = State { stack = [], ops = p, ptr = 0, output = [], exit = False }

out :: State -> String -> State
out s o = s { output = output s ++ [o] }

err :: State -> String -> State
err s m = s { output = ("Error: " ++ m) : output s, exit = True }

isEnd :: State -> Bool
isEnd s = ptr s == length (ops s)

getOp :: State -> String
getOp s = ops s !! ptr s

step :: State -> State
step s = s { ptr = ptr s + 1 }

push :: Value -> State -> State
push v s = s { stack = v : stack s }

pop :: State -> (Value, State)
pop s = case stack s of
    []       -> (Nil, err s "Stack underflow")
    (x : xs) -> (x, s { stack = xs })

showState :: State -> String
showState s = concat $ reverse $ output s