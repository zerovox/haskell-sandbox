import Data.List
import Data.Maybe

-- A solution for http://programmingpraxis.com/2012/12/14/115132219018763992565095597973971522401/

-- Check the n-digit number to see if it is narcissistic for any permuation of the n bits. So "check 3 351 == True"
-- We know xs is in increasing order, so it is already sorted
check n xs p	= if xs == reverse (sort (contract x)) then Just x else Nothing
	where x 	= sum (map (p!!) xs)
-- Converts LSB first numbers into Integers
expand []		= 0
expand (x:xs) 	= x+(10* (expand xs))
contract 0 		= []
contract n 		= n `mod` 10 : contract (n `div` 10)

-- Lists all nondecreasing numbers
allNumbers 0	= [(0,[0])]
allNumbers 1	= map (\x -> (x, [x])) [0..9]
allNumbers n 	= concat (map (\(z,x) -> map (\y -> (y,(y:x))) [z..9]) (allNumbers (n-1)))
allNs n			= map snd (allNumbers n)

-- We can speed up checking by caching [0..9] to the power of n
powers n 		= map (^n) [0..9]
-- List narcissistic numbers of length n
narcissistic n 	= catMaybes (map (\xs ->check n xs (powers n)) (allNs n))

narcsUpTo n 	= concat (map narcissistic [0..n])