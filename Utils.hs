module Utils where

import Data.Char

shownum n = show n

hd :: [a] -> a
hd = head

tl :: [a] -> [a]
tl = tail

zip2 :: [a] -> [b] -> [(a, b)]
zip2 = zip

space :: Int -> String
space 0 = ""
space n = space (n -1) ++ " "

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')
isWhiteSpace c = c `elem` " \t\n"

numval :: String -> Int
numval = foldl (\a c -> 10 * a + ord c - ord '0') 0

-- assoc list
type ASSOC a b = [(a, b)]

aLookup [] k' def = def
aLookup ((k, v) : bs) k' def
  | k == k' = v
  | otherwise = aLookup bs k' def

aDomain :: ASSOC a b -> [a]
aDomain alist = [key | (key, val) <- alist]

aRange :: ASSOC a b -> [b]
aRange alist = [val | (key, val) <- alist]

aEmpty :: ASSOC a b
aEmpty = []

-- gc heap
type Addr = Int

--         obj数目   未使用addr 使用的
type Heap a = (Int, [Int], [(Int, a)])

hInitial :: Heap a
hInitial = (0, [1 ..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, (next : free), cts) n = ((size + 1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a, n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size -1, a : free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (size, free, cts) a =
  aLookup cts a (error ("can't find node " ++ showaddr a ++ " in heap"))

hAddresses :: Heap a -> [Addr]
hAddresses (size, free, cts) = [addr | (addr, node) <- cts]

hSize :: Heap a -> Int
hSize (size, fre, cts) = size

hNull :: Addr
hNull = 0

hIsnull :: Addr -> Bool
hIsnull a = a == 0

showaddr :: Addr -> [Char]
showaddr a = "#" ++ shownum a

remove :: [(Int, a)] -> Int -> [(Int, a)]
remove [] a = error ""
remove ((a', n) : cts) a
  | a == a' = cts
  | otherwise = (a', n) : remove cts a

-- generate names
type NameSupply = Int

initialNameSuppy :: NameSupply
initialNameSuppy = 0

getName :: NameSupply -> String -> (NameSupply, String)
getName name_supply prefix = (name_supply + 1, makeName prefix name_supply)

getNames :: NameSupply -> [String] -> (NameSupply, [String])
getNames name_supply prefixes =
  (name_supply + length prefixes, zipWith makeName prefixes [name_supply ..])

makeName :: String -> Int -> String
makeName prefix ns =
  prefix ++ "_" ++ shownum ns

-- set

type Set a = [a]

setEmpty :: Set a
setEmpty = []

setIsEmpty :: Set a -> Bool
setIsEmpty s = null s

setSingleton :: a -> Set a
setSingleton x = [x]

setFromList ::(Eq a, Ord a)=> [a] -> Set a
setFromList xs = rmdup (sort xs)
                    where rmdup [] = []
                          rmdup [x] = [x]
                          rmdup (x:y:xs) | x == y = rmdup (y:xs)
                                    | otherwise = x:rmdup (y:xs)
setToList :: Set a -> [a]
setToList xs = xs

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion [] [] = []
setUnion [] (b:bs) = (b:bs)
setUnion (a:as) [] = (a:as)
setUnion (a:as) (b:bs) | a < b = a:setUnion as (b:bs)
                       | a == b = a:setUnion as bs
                       | a > b = b:setUnion (a:as) bs

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection [] [] = []
setIntersection [] (b:bs) = []
setIntersection (a:as) [] = []
setIntersection (a:as) (b:bs) 
                       | a < b = setIntersection as (b:bs)
                       | a == b = a:setIntersection as bs
                       | a > b =setIntersection (a:as) bs

setSubtraction :: Ord a => Set a -> Set a -> Set a
setSubtraction [] [] = []
setSubtraction [] (b:bs) = []
setSubtraction (a:as) [] = (a:as)
setSubtraction (a:as) (b:bs) 
                       | a < b = a:setIntersection as (b:bs)
                       | a == b = setIntersection as bs
                       | a > b =setIntersection (a:as) bs


setElementOf :: Ord t => t -> Set t -> Bool
setElementOf x [] = False 
setElementOf x (y:ys) = x == y || (x > y && setElementOf x ys)

setUnionList :: Ord a => [Set a] -> Set a
setUnionList ss  = foldl setUnion setEmpty ss


-- common

first (a, b) = a
second (a, b) = b

-- map + foldll
mapAccuml::(a -> b -> (a, c))
          -> a
          -> [b]
          -> (a, [c])
mapAccuml f acc [] = (acc, [])          
mapAccuml f acc (x:xs) = (acc2, x':xs')
                            where (acc1, x') = f acc x  
                                  (acc2, xs') = mapAccuml f acc1 xs


foldll :: (a -> b -> a) -> a->[b] -> a
foldll = foldl

sort :: Ord a => [a] -> [a]
sort [] = []
sort [x] = [x]
sort (x:xs) = [y | y <- xs, y < x] ++ x:[y|y<-xs, y>=x] 