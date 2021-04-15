module Language where

import Data.Char
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
  '\n' : space indent ++ (flatten indent seqs)
flatten col ((IIndent seq, indent) : seqs) =
  flatten col ((seq, col) : seqs)
flatten col ((IStr s, indent) : seqs) =
  s ++ flatten (col + length s) seqs
flatten col ((INil, indent) : seqs) =
  flatten col seqs
flatten col ((IAppend seq1 seq2, indent) : seqs) =
  flatten col ((seq1, indent) : (seq2, indent) : seqs)
flatten col [] = ""

iDisplay :: Iseq -> String
iDisplay seq = flatten 0 [(seq, 0)]

-- helper
iConcat :: [Iseq] -> Iseq
iConcat = foldr iAppend iNil

iSpace :: Iseq
iSpace = iStr " "

iNum :: Int -> Iseq
iNum n = iStr (show n)

iInterleave :: Iseq -> [Iseq] -> Iseq
iInterleave sep [] = iNil
iInterleave sep [seq] = seq
iInterleave sep (seq : seqs) =
  seq `iAppend` (sep `iAppend` iInterleave sep seqs)

iFWNum :: Int -> Int -> Iseq
iFWNum width n =
  iStr (space (width - length digits) ++ digits)
  where
    digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs =
  iConcat (map lay_item (zip [1 ..] seqs))
  where
    lay_item (n, seq) =
      iConcat
        [ iFWNum 4 n,
          iStr ")",
          iIndent seq,
          iNewline
        ]

-- print Expr

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iNum n
pprExpr (EVar v) = iStr v
pprExpr (EAp (EAp (EVar "+") e1) e2) = iConcat [pprAExpr e1, iStr " + ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "-") e1) e2) = iConcat [pprAExpr e1, iStr " - ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "*") e1) e2) = iConcat [pprAExpr e1, iStr " * ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "/") e1) e2) = iConcat [pprAExpr e1, iStr " / ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "<") e1) e2) = iConcat [pprAExpr e1, iStr " < ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "<=") e1) e2) = iConcat [pprAExpr e1, iStr " <= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "==") e1) e2) = iConcat [pprAExpr e1, iStr " == ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "~=") e1) e2) = iConcat [pprAExpr e1, iStr " ~= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar ">=") e1) e2) = iConcat [pprAExpr e1, iStr " >= ", pprAExpr e2]
pprExpr (EAp (EAp (EVar ">") e1) e2) = iConcat [pprAExpr e1, iStr " > ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "&") e1) e2) = iConcat [pprAExpr e1, iStr " & ", pprAExpr e2]
pprExpr (EAp (EAp (EVar "|") e1) e2) = iConcat [pprAExpr e1, iStr " | ", pprAExpr e2]
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
pprExpr (ECase e alts) =
  iConcat
    [ iStr "case ",
      pprExpr e,
      iStr " of",
      iNewline,
      iStr " ",
      iIndent (iInterleave iNl (map pprAlt alts))
    ]
  where
    iNl = iConcat [iStr ";", iNewline]
    pprAlt (tag, args, rhs) =
      iConcat
        [ iStr "<",
          iNum tag,
          iStr "> ",
          pprArgs args,
          iStr " -> ",
          iIndent (pprExpr rhs)
        ]

pprAExpr e | isAtomicExpr e = pprExpr e
pprAExpr e = iConcat [iStr "(", pprAExpr e, iStr ")"]

pprDefns :: [(Name, CoreExpr)] -> Iseq
pprDefns defns =
  iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> Iseq
pprDefn (name, expr) =
  iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprArgs :: [Name] -> Iseq
pprArgs args = iInterleave iSpace (map iStr args)

pprSc :: CoreScDefn -> Iseq
pprSc (name, args, body) =
  iConcat
    [ iStr name,
      iSpace,
      pprArgs args,
      iStr " = ",
      iIndent (pprExpr body)
    ]

pprProgram :: CoreProgram -> Iseq
pprProgram prog =
  iInterleave
    (iAppend (iStr " ;") iNewline)
    (map pprSc prog)

pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

-- parser

type Token = String

twoCharOps :: [String]
twoCharOps =
  [ "==",
    "~=",
    ">=",
    "<=",
    "->"
  ]

clex :: String -> [Token]
clex (c : cs) | isWhiteSpace c = clex cs
clex (c : cs) | isDigit c = num_token : clex rest_cs
  where
    num_token = c : takeWhile isDigit cs
    rest_cs = dropWhile isDigit cs
clex (c : cs) | isAlpha c = var_tok : clex rest_cs
  where
    var_tok = c : takeWhile isIdChar cs
    rest_cs = dropWhile isIdChar cs
-- comment
clex ('|' : '|' : cs) = clex (dropWhile (/= '\n') cs)
clex (c1 : c2 : cs) | ([c1, c2] `elem` twoCharOps) = [c1, c2] : clex cs
clex (c : cs) = [c] : clex cs
clex [] = []

-- parser
type Parser a = [Token] -> [(a, [Token])]

-- pLit :: String -> Parser String
-- pLit s (tok : toks) | s == tok = [(s, toks)]
-- pLit s toks = []

-- pVar :: Parser String
-- pVar (tok : toks) = [(tok, toks)]
-- pVar [] = []

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1
  ]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1,
      (v3, toks3) <- p3 toks2
  ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (combine v1 v2 v3 v4, toks4)
    | (v1, toks1) <- p1 toks,
      (v2, toks2) <- p2 toks1,
      (v3, toks3) <- p3 toks2,
      (v4, toks4) <- p4 toks3
  ]

pEmpty :: a -> Parser a
pEmpty s toks = [(s, toks)]

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks = [(f v1, toks1) | (v1, toks1) <- p toks]

pOneOrMoreWithSep :: Parser a -> Parser p -> Parser [a]
pOneOrMoreWithSep p psep = pThen (:) p (pOneOrMoreWithSep_c p psep)

pOneOrMoreWithSep_c :: Parser a -> Parser p -> Parser [a]
pOneOrMoreWithSep_c p psep =
  (pThen discard_sep psep (pOneOrMoreWithSep p psep)) `pAlt` (pEmpty [])
  where
    discard_sep sep vs = vs

pSat :: (String -> Bool) -> Parser String
pSat pred [] = []
pSat pred (tok : toks)
  | pred tok = [(tok, toks)]
  | otherwise = []

pLit :: String -> Parser String
pLit s = pSat (== s)

keywords :: [String]
keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pVar :: Parser String
pVar = pSat isVar
  where
    isVar s = isAlpha (head s) && s `notElem` keywords

pNum :: Parser Int
pNum = pSat (isDigit . head) `pApply` numval

-- parse Core Program

syntax :: [Token] -> CoreProgram
syntax = error ""

parse :: String -> CoreProgram
parse = syntax . clex

-- parse filename =
--   syntax (lex (read filename))

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

pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")

pGreeting = pThen mk_pair pHelloOrGoodbye pVar
  where
    mk_pair hg name = (hg, name)

pGreeting1 =
  pThen
    keep_first
    (pThen mk_pair pHelloOrGoodbye pVar)
    (pLit "!")
  where
    keep_first hg_name exclamation = hg_name
    mk_pair hg name = (hg, name)

pGreeting2 =
  pThen3
    mk_greeting
    pHelloOrGoodbye
    pVar
    (pLit "!")
  where
    mk_greeting hg name exclamation = (hg, name)

pGreetings :: Parser [(String, String)]
pGreetings = pZeroOrMore pGreeting

pGreetingN :: Parser Int
pGreetingN = (pZeroOrMore pGreeting) `pApply` length