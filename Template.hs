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

data Node
  = NAp Addr Addr
  | --        document/debug
    NSupercomb Name [Name] CoreExpr
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

--  (stack, dump, heap, globals)
-- statk - stack of address
-- dump -- 暂时不用
-- heap -- 手机各种node

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) =
  (stack, dump, heap, sc_defs, stats_fun stats)

extraPreludeDefs = []

buildInitialHeap :: [CoreScDefn] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = mapAccuml allocateSc hInitial sc_defs

allocateSc :: TiHeap -> CoreScDefn -> (TiHeap, (Name, Addr))
allocateSc heap (name, args, body) =
  (heap', (name, addr))
  where
    (heap', addr) = hAlloc heap (NSupercomb name args body)

compile :: CoreProgram -> TiState
compile program =
  (initial_stack, initialTiDump, initial_heap, globals, tiStatInitial)
  where
    sc_defs = program ++ preludeDefs ++ extraPreludeDefs
    (initial_heap, globals) = buildInitialHeap sc_defs
    initial_stack = [address_of_main]
    address_of_main = aLookup globals "main" (error "main is not defined")

eval :: TiState -> [TiState]
eval state = state : rest_states
  where
    rest_states
      | tiFinal state = []
      | otherwise = eval next_state
    next_state = doAdmin (step state)

doAdmin :: TiState -> TiState
doAdmin state = applyToStats tiStatIncSetps state

tiFinal :: TiState -> Bool
tiFinal ([sole_addr], dump, heap, gloabls, stats) =
  isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum n) = True
isDataNode node = False

step :: TiState -> TiState
step state =
  dispatch (hLookup heap (hd stack))
  where
    (stack, dump, heap, globals, stats) = state
    dispatch (NNum n) = numStep state n
    dispatch (NAp a1 a2) = apStep state a1 a2
    dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TiState -> Int -> TiState
numStep state n = error "Number applied as function"

apStep :: TiState -> Addr -> Addr -> TiState
apStep (stack, dump, heap, globals, states) a1 a2 =
  (a1 : stack, dump, heap, globals, states)

scStep :: TiState -> Name -> [Name] -> CoreExpr -> TiState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body =
  (new_stack, dump, new_heap, globals, stats)
  where
    new_stack = result_addr : (drop (length arg_names + 1) stack)
    (new_heap, result_addr) = instantiate body heap env
    env = arg_bindings ++ globals
    arg_bindings = zip2 arg_names (getargs heap stack)

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (sc : stack) =
  map get_arg stack
  where
    get_arg addr = arg where (NAp fun arg) = hLookup heap addr

instantiate ::
  CoreExpr ->
  TiHeap ->
  ASSOC Name Addr ->
  (TiHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env =
  hAlloc heap2 (NAp a1 a2)
  where
    (heap1, a1) = instantiate e1 heap env
    (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env =
  (heap, aLookup env v (error ("Undefined name" ++ show v)))
instantiate (EConstr tag arity) heap env =
  instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env =
  instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env =
  error "Can't instantiate case expr"

instantiateConstr :: Int -> Int -> TiHeap -> ASSOC Name Addr -> (TiHeap, Addr)
instantiateConstr tag arity heap env =
  error "Can't "

instantiateLet ::
  IsRec ->
  [(Name, Expr Name)] ->
  Expr Name ->
  TiHeap ->
  ASSOC Name Addr ->
  (TiHeap, Addr)
instantiateLet isrec defs body heap env =
  error "Can't "

showResults :: [TiState] -> String
showResults states =
  iDisplay
    ( iConcat
        [ iLayn (map showState states),
          showStats (last states)
        ]
    )

showState :: TiState -> Iseq
showState (stack, dump, heap, globals, stats) =
  iConcat [showStack heap stack, iNewline]

showStack :: TiHeap -> TiStack -> Iseq
showStack heap stack =
  iConcat
    [ iStr "Stk [",
      iIndent (iInterleave iNewline (map show_stack_item stack)),
      iStr " ]"
    ]
  where
    show_stack_item addr =
      iConcat
        [ showFWAddr addr,
          iStr ";",
          showStkNode heap (hLookup heap addr)
        ]

showSktNode :: TiHeap -> Node -> Iseq
showSktNode heap (NAp fun_addr arg_addr) =
  iConcat
    [ iStr "NAp ",
      showFWAddr fun_addr,
      iStr " ",
      showFWAddr arg_addr,
      iStr " (",
      showNode (hLookup heap arg_addr),
      iStr ")"
    ]

showStkNode heap node = showNode node

showNode :: Node -> Iseq
showNode (NAp a1 a2) =
  iConcat
    [ iStr "NAp ",
      showAddr a1,
      iStr " ",
      showAddr a2
    ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = (iStr "NNum") `iAppend` (iNum n)

showAddr :: Addr -> Iseq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> Iseq
showFWAddr addr =
  iStr (space (4 - length str) ++ str)
  where
    str = show addr

showStats :: TiState -> Iseq
showStats (stack, dump, heap, globals, stats) =
  iConcat
    [ iNewline,
      iNewline,
      iStr "Total number of steps = ",
      iNum (tiStatGetSetps stats)
    ]

runProg :: String -> String
runProg = showResults . eval . compile . parse
