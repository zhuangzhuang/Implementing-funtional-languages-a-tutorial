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