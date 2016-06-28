main = do
    print (mybc 5 3)		-- 10
    print (mybc 49 6)		-- 13983816
    print (myheron 5 3 4)	-- 6
    print (mysumevens 1 6)	-- 12
    print (myreverse 345)	-- 543
    print (myreverse 7)		-- 7
    print (mypalindrome 222322)	-- False
    print (mypalindrome 2123232)-- False
    print (myprime 2)		-- True
    print (myprime 12)		-- False
    print (mysumprimes 1 10)	-- 17
    print (mysumprimes 2 3)	-- 5
    print (mysumprimes 90 96)	-- 0
    print (myavgpr 2 3)		-- 2.5
    print (myavgpr 7 10)	-- 7
    
-- Problem 1
mybc n k = (factorial n)/((factorial k)*(factorial (n - k)))
factorial n = if(n == 1) then 1 else (n*(factorial (n - 1)))

-- Problem 2
myheron a b c = 1/4*(sqrt(2*((a^2)*(b^2) + (a^2)*(c^2) + (b^2)*(c^2)) - (a^4 + b^4 + c^4)))

-- Problem 3
mysumevens a b
    | a > b	    = 0
    | mod a 2 == 0  = a + mysumevens (a + 1) b
    | otherwise     = mysumevens (a + 1) b

-- Problem 4
myreverse x = read (reverse (show x)) :: Int

-- Problem 5
mypalindrome x = if(x == myreverse x) then True else False

-- Problem 6
myprime x = if (x == 1) then False else myprimehelper 2 x
myprimehelper start end
    | start >= end 		= True
    | (mod end start == 0)	= False
    | otherwise			= myprimehelper (start + 1) end

-- Problem 7
mysumprimes a b 
    | a > b	= 0
    | myprime a = a + mysumprimes (a+1) b 
    | otherwise = mysumprimes (a+1) b

-- Problem 8
myavgpr :: Int -> Int -> Float
myavgpr a b = fromIntegral (mysumprimes a b)/(myprcounter a b)
myprcounter a b
    | a > b	= 0
    | myprime a = 1 + myprcounter (a+1) b
    | otherwise = myprcounter (a+1) b
