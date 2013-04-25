-- A solution for http://programmingpraxis.com/2012/12/11/stepwise-program-development-a-heuristic-algorithm

-- Splits xs into the top n characters and the next n characters
split xs n	= (take n xs, take n (drop n xs))
-- Given ys is legal, checks that (y:ys) is also legal
check xs	= foldl (\a (ys, zs) -> if ys == zs then True else a) False (map (split xs) [1..((length xs) `div` 2)])

-- The alphabet from which we draw the characters
alphabet 	= [1,2,3]
-- Generate all sequences of length n
generate 0 	= [[]]
generate n 	= foldl (\ys zs -> if check zs then ys else zs:ys) [] (concat ( map (\x -> map (\y -> y:x) alphabet) (generate (n-1))))