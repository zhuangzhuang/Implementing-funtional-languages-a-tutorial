module Template where

import Language
import Utils

-- n*m
-- (n m d t)
-- n m  d-1 t+1
-- where d > 0

type MultState = (Int, Int, Int, Int)

stepMult :: MultState -> MultState
stepMult (n, m, d, t) | d > 0 = (n, m, d -1, t + 1)
stepMult (n, m, d, t) | d == 0 = (n, m -1, n, t)

mutltFinal :: MultState -> Bool
mutltFinal (n, m, d, t)
  | m == 0 && d == 0 = True
  | otherwise = False

evalMul :: MultState -> [MultState]
evalMul state =
  if mutltFinal state
    then [state]
    else state : evalMul (stepMult state)

--  (stack, dump, heap, globals)
-- statk - stack of address
--

data Node
  = NAp Addr Addr
  | NSupercomb Name [Name] CoreExpr
  | NNum Int

type TiGlobals = ASSOC Name Addr

type TiStats = Int

tiStatInitial :: TiStats
tiStatInitial = 0

tiStatIncSetps :: TiStats -> TiStats
tiStatIncSetps s = s + 1

tiStatGetSetps :: TiStats -> Int
tiStatGetSetps s = s

type TiStack = [Addr]

data TiDump = DummyTiDump

initialTiDump = DummyTiDump

type TiHeap = Heap Node

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) =
  (stack, dump, heap, sc_defs, stats_fun stats)

compile :: CoreProgram -> TiState
compile = error "asdf"

eval :: TiState -> [TiState]
eval = error "asdf"

showResults :: [TiState] -> String
showResults = error "asdf"

runProg :: String -> String
runProg = showResults . eval . compile . parse