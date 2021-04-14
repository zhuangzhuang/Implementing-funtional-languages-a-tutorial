module Language where

import Utils

type Name = String

data Expr a
  = EVar Name
  | ENum Int
  | EConstr Int Int
  | EAp (Expr a) (Expr a)
  | ELet
      IsRec
      [(a, Expr a)]
      (Expr a)
  | ECase
      (Expr a)
      [Alter a]
  | ELam [a] (Expr a)
  deriving (Show)

type CoreExpr = Expr Name

type IsRec = Bool

recursive, noRecursive :: IsRec
recursive = True
noRecursive = False

bindersOf :: [(a, b)] -> [a]
bindersOf defns = [name | (name, rhs) <- defns]

rhssOf :: [(a, b)] -> [b]
rhssOf defns = [rhs | (name, rhs) <- defns]

type Alter a = (Int, [a], Expr a)

type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum v) = True
isAtomicExpr e = False

type Program a = [ScDefn a]

type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)

type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ( "S",
      ["f", "g", "x"],
      EAp
        (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x"))
    ),
    ( "compose",
      ["f", "g", "x"],
      EAp
        (EVar "f")
        ( EAp
            (EVar "g")
            (EVar "x")
        )
    ),
    ( "twice",
      ["f"],
      EAp
        ( EAp
            (EVar "compose")
            (EVar "f")
        )
        (EVar "f")
    )
  ]

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldl EAp e1 (take n e2s)
  where
    e2s = e2 : e2s

-- pprint :: CoreProgram -> String
-- pprint = error "ad"

-- pretty-print string

-- pprExpr :: CoreExpr -> String
-- pprExpr (ENum n) = show n
-- pprExpr (EVar v) = v
-- pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprAExpr e2

-- pprAExpr :: CoreExpr -> String
-- pprAExpr e | isAtomicExpr e = pprExpr e
-- pprAExpr e = "(" ++ pprExpr e ++ ")"

-- pretty-print new

data Iseq
  = INil
  | IStr String
  | IAppend Iseq Iseq
  | IIndent Iseq
  | INewLine

iNil :: Iseq
iNil = INil

iStr :: String -> Iseq
iStr = IStr

iAppend :: Iseq -> Iseq -> Iseq
iAppend = IAppend

iNewline :: Iseq
iNewline = INewLine

iIndent :: Iseq -> Iseq
iIndent = IIndent

-- helper
-- flatten :: [Iseq] -> String
-- flatten [] = ""
-- flatten (INil : seqs) = flatten seqs
-- flatten (IStr s : seqs) = s ++ (flatten seqs)
-- flatten (IAppend seq1 seq2 : seqs) = flatten (seq1 : seq2 : seqs)

flatten ::
  Int -> -- column
  [(Iseq, Int)] -> -- work list
  String -- result
flatten col ((INewLine, indent) : seqs) =
  '\n' : (space indent) ++ (flatten indent seqs)

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

-- helper
iConcat :: [Iseq] -> Iseq
iConcat = error ""

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave = error ""

pprExpr :: CoreExpr -> Iseq
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) =
  (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprAExpr e2)
pprExpr (ELet isrec defns expr) =
  iConcat
    [ iStr keyword,
      iNewline,
      iStr " ",
      iIndent (pprDefns defns),
      iNewline,
      iStr "in ",
      pprExpr expr
    ]
  where
    keyword
      | not isrec = "let"
      | isrec = "letrec"

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns =
  iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) =
  iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-- Examples

-- main = double 21;
-- double x = x + x;
p :: CoreProgram
p =
  [ ("main", [], EAp (EVar "double") (ENum 21)),
    ( "double",
      ["x"],
      EAp
        ( EAp
            (EVar "+")
            (EVar "x")
        )
        (EVar "x")
    )
  ]

e1 = EVar "a"

e2 = mkMultiAp 10 (EVar "f") (EVar "x")