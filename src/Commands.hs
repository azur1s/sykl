module Commands where

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
    -- Same type numbers
    (Int i, Int j)     -> Right $ Int (i + j)
    (Float i, Float j) -> Right $ Float (i + j)
    -- Different type numbers
    (Int i, Float j) -> Right $ Float (fromIntegral i + j)
    (Float i, Int j) -> Right $ Float (i + fromIntegral j)
    -- String concatenation
    (String i, String j) -> Right $ String (i ++ j)
    (String i, Int j)    -> Right $ String (i ++ show j)
    (Int i, String j)    -> Right $ String (show i ++ j)
    (String i, Bool j)   -> Right $ String (i ++ show j)
    (Bool i, String j)   -> Right $ String (show i ++ j)
    (String i, List j)   -> Right $ String (i ++ show j)
    (List i, String j)   -> Right $ String (show i ++ j)
    -- Join lists
    (List i, List j) -> Right $ List (i ++ j)
    _              -> Left $ err "Type error" vm) vm

sub :: VM -> VM
sub vm = apply2 (\x y -> case (x, y) of
    (Int i, Int j) -> Right $ Int (j - i)
    _              -> Left $ err "Type error" vm) vm

mul :: VM -> VM
mul vm = apply2 (\x y -> case (x, y) of
    (Int i, Int j) -> Right $ Int (i * j)
    _              -> Left $ err "Type error" vm) vm

div :: VM -> VM
div vm = apply2 (\x y -> case (x, y) of
    (Int i, Int j) -> Right $ Int (Prelude.div j i)
    _              -> Left $ err "Type error" vm) vm

putv :: VM -> VM
putv vm = apply (\x -> Left $ out (fmt x) vm) vm

-- | Executing

step :: VM -> VM
step vm = do
    let instr = instrs vm !! ip vm
    let c = code instr
    let vm' = case c of
            Op op -> case op of
                "." -> putv vm
                "+" -> add vm
                "-" -> sub vm
                "*" -> mul vm
                "/" -> Commands.div vm
                _   -> err ("Unknown instruction: " ++ op) vm
            Push x -> push x vm
    vm' { ip = ip vm' + 1 }

exec :: VM -> VM
exec vm = if panic vm
    then vm
    else if ip vm >= length (instrs vm)
        then vm
        else exec (step vm)