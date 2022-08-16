module Main where

import Types
import Func

main :: IO ()
main = do
    let s = initWith ["34", "35", "+", "#"]

    putStr $ showState $ runAll s