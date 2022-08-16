module Main where

import VM
import Instruction

main :: IO ()
main = do
    let vm = initVM ["34", "35", "+", "#"]
    let vm' = exec vm
    putStr $ finalize vm'