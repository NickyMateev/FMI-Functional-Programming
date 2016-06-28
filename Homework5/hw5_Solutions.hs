main = do
  print (replac [1,2,3] [(2,3), (4,5)])
  print (replac "batiisne" [('b','T'), ('i','o'), ('s','i')])
  print (permut [1,2])

-- Problem 1
replac :: (Eq t) => [t] -> [(t, t)] -> [t]
replac list dict = replacH list dict dict

replacH :: (Eq t) => [t] -> [(t, t)] -> [(t, t)] -> [t]
replacH [] _ dict   = []
replacH xs []  dict = xs
replacH (x:xs) (y:ys) dict
  | x == (fst y)             = (snd y) : (replacH xs (y:ys) dict)
  | x /= (fst y) && null ys  = x : (replacH xs dict dict)
  | otherwise                = replacH (x:xs) ys dict

-- Problem 2
permut :: (Eq t) => [t] -> [[t]]
permut [] = [[]]
permut xs = [x:ys | x <- xs, ys <- permut (delete x xs)]

delete :: (Eq a) => a -> [a] -> [a]
delete _ [] = []
delete el (x:xs)
  | el == x     = xs
  | otherwise   = x : delete el xs
