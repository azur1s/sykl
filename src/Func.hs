module Func where

import Text.Read (readMaybe)
import Types

-- Int

add :: State -> State
add s = do
    let (a, s') = pop s
    let (b, s'') = pop s'
    case (a, b) of
        (Int a', Int b') -> push (Int (a' + b')) s''
        _ -> err s'' ("Can't add " ++ show a ++ " and " ++ show b)

-- Generic

put :: State -> State
put s = do
    let (a, s') = pop s
    out s' (fmt a)

-- All

run :: String -> State -> State
run op s = case op of
    "#" -> put s
    "+" -> add s
    _   -> case readMaybe op :: Maybe Int of
        Just n  -> push (Int n) s
        Nothing -> err s ("Unknown operation: " ++ op)

run' :: State -> State
run' s = do
    if isEnd s then s else do
        let op = getOp s
        run op s

runAll :: State -> State
runAll s = if isEnd s
    then s
    else do
        runAll (step (run' s))