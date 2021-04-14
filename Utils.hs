module Utils where

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