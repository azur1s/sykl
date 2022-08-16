module Test where

import Data.Array.IO

data S = S { a :: IOArray Int Int }

main = do
    arr <- newArray (0, 10) 0
    let s = S { a = arr }
    writeArray (a s) 0 42
    a <- readArray (a s) 0
    print a