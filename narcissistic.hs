-- A solution for http://programmingpraxis.com/2012/12/14/115132219018763992565095597973971522401/

-- Check the n-digit number to see if it is narcissistic. Numbers stored LSB first. So "check 3 351 == True"
check n xs		= if (expand xs) == sum (map (\x -> x^n) xs) then True else False
-- Converts LSB first numbers into Integers
expand []		= 0
expand (x:xs) 	= x+(10* (expand xs))

-- Lists all numbers of length n in LSB first reperesentation
allNumbers 0	= [[0]]
allNumbers 1	= map (\x -> [x]) [1..9]
allNumbers n 	= concat (map (\x -> map (\y -> y:x) [0..9]) (allNumbers (n-1)))
-- List narcissistic numbers of length n
narcissistic n 	= filter (check n) (allNumbers n)

allNarcs 		= map expand (concat (map narcissistic [0..]))