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