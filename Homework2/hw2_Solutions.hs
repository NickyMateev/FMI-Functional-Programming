main = do
    print (convert 13 10 2)
    print (primeReorder [2,3,4,5,6])

-- Problem 1
converterToDec :: Int -> Int -> Int -> Int
converterToDec 0 base index = 0
converterToDec num base index = (num `mod` 10)*(base^index) + converterToDec (num `div` 10) base (index + 1)

converterDecToOtherBase :: Int -> Int -> Int
converterDecToOtherBase 0 base = 0
converterDecToOtherBase num 10 = num
converterDecToOtherBase num base = (num `mod` base) + 10 * converterDecToOtherBase(num `div` base) base

convert :: Int -> Int -> Int -> Int
convert num startBase endBase = converterDecToOtherBase (converterToDec num startBase 0) endBase


-- Problem 2
primeReorder :: [t] -> [t]
primeReorder xs = (primeNumbersIndex xs 2) ++ (compositeNumbersIndex xs 2)

myprime x = if (x == 1) then False else myprimehelper 2 x
myprimehelper start end
    | start >= end 		= True
    | (mod end start == 0)	= False
    | otherwise			= myprimehelper (start + 1) end


primeNumbersIndex :: [t] -> Int -> [t]
primeNumbersIndex [] index = []
primeNumbersIndex (x:xs) index = if(myprime index == True) then x:(primeNumbersIndex xs (index + 1))
				    else primeNumbersIndex xs (index + 1)
				    
compositeNumbersIndex :: [t] -> Int -> [t]
compositeNumbersIndex [] index = []
compositeNumbersIndex (x:xs) index = if(myprime index == False) then x:(compositeNumbersIndex xs (index + 1))
				    else compositeNumbersIndex xs (index + 1)
