-- The obvious quicksort implementation in Haskell requires iterating over both lists and filtering out those above and below the pivot.
-- If you build both lists at once, you only need to iterate over the list once.


-- An infinite list of fairly evenly distributed numbers
inflist = [x*(x+5000) `mod` 10000 | x <- [1..1000000]]
biglist = take 100000 inflist 

-- The obvious quicksort implementation
qs [] = []
qs (x:xs) = qs ys ++ [x] ++ qs zs
    where ys = [a | a <- xs, a <= x]
          zs = [b | b <- xs, b > x]

-- Building both lists at once
tqs [] = []
tqs (x:xs) 	= tqs ys ++ (x : tqs zs)
	where 	(ys, zs)	= foldl ds ([],[]) xs
		ds (ys, zs) z 	= if z < x then (z:ys, zs) else (ys, z:zs) 	