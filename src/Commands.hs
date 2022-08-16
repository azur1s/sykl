module Commands where

import Text.Read (readMaybe)
import VM

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

-- | Executing

step :: VM -> VM
step vm = do
    let instr = instrs vm !! ip vm
    let c = code instr
    let vm' = case c of
            "$" -> putv vm
            "+" -> add vm
            _   -> case readMaybe c :: Maybe Int of
                Just n  -> push (Int n) vm
                Nothing -> err ("Unknown instruction: " ++ c) vm
    vm' { ip = ip vm' + 1 }

exec :: VM -> VM
exec vm = if panic vm
    then vm
    else if ip vm >= length (instrs vm)
        then vm
        else exec (step vm)