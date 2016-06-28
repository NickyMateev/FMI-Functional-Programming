main = do
	print (intersect [1,2,2,2] [1,2,2])

-- Problem 1
countPalindromes :: Int -> Int -> Int
countPalindromes a b
	| a > b		= 0
	| otherwise = if (mypalindrome a) then 1 + countPalindromes (a + 1) b else countPalindromes (a + 1) b 

myreverse :: Int -> Int
myreverse x = read (reverse (show x)) :: Int

mypalindrome :: Int -> Bool
mypalindrome x = if(x == myreverse x) then True else False

--Problem 2
countmin :: [Int] -> Int
countmin [] = 0
countmin xs = counterMin (findMin (head xs) xs) xs

findMin :: Int -> [Int] -> Int
findMin a [] = a
findMin a (x:xs)
	| a > x		= findMin x xs
	| otherwise = findMin a xs

counterMin :: Int -> [Int] -> Int
counterMin a []	= 0
counterMin a (x:xs)
	| a == x	= 1 + counterMin a xs
	| otherwise = counterMin a xs

--Problem 3
intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = intersectHelper (qSort xs) (qSort ys)

intersectHelper :: [Int] -> [Int] -> [Int]
intersectHelper [] _ = []
intersectHelper _ [] = []
intersectHelper (x:xs) (y:ys)
	| x==y		= x: intersectHelper xs ys
	| x < y 	= intersectHelper xs (y:ys)
	| otherwise = intersectHelper (x:xs) ys 

qSort :: [Int] -> [Int]
qSort []	 = []
qSort (x:xs) = qSort a ++ [x] ++ qSort b
	where
		a = [y | y <- xs, y<= x]
		b = [y | y <- xs, y > x]
		
-- Problem 4
list2primes :: [Int] -> [(Int, Int)]
list2primes xs = list2primesHelper 2 (evenElements xs)  

list2primesHelper :: Int -> [Int] -> [(Int, Int)]
list2primesHelper n [] = []
list2primesHelper n (x:xs)
	| myprime n && myprime (x-n) = [(n, x-n)] ++ list2primesHelper 2 xs
	| otherwise				     = list2primesHelper (n+1) (x:xs)


evenElements :: [Int] -> [Int]
evenElements xs = [x | x <- xs, even x]

myprime x = if (x == 1) then False else myprimehelper 2 x
myprimehelper start end
    | start >= end 			= True
    | (mod end start == 0)	= False
    | otherwise				= myprimehelper (start + 1) end
