```haskell
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
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name
type Program a = [ScDefn a]
type CoreProgram = Program Name
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name
```

isRed c = case c of 
    <1> -> True;
    <2> -> False;
    <3> -> False

depth t = case t of
    <1> n -> 0;
    <2> t1 t2 -> 1 + max(depth t1) (depth t2)

expr ->   let defns in expr
        | letrec defns in expr
        | case expr of alts
        | \var1 ... varn . expr
        | expr1

expr1 -> expr2 | expr1
        | expr2
expr2 -> expr3 & expr2
        expr3
expr3 -> expr4 relop expr4
        expr4
expr4 -> expr5 + expr4
        expr5 - expr4
        expr5
expr5 -> expr6 * expr5
        expr6 / expr5
        expr6
expr6 -> aexpr1 ... aexprn
