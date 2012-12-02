-- A solution for http://programmingpraxis.com/2012/11/27/amazon-interview-question/
-- Insert takes O(100) as there will be at most 100 comparisons. 
-- Therefore the main loop takes O(|xs|*100)
-- A possibly faster aproach would use a modified quicksort, where you partition the list, and only recurse on the lhs as long as it is larger than 100 elements, otherwise repeat on the rhs with 100 - the size of the lhs. TODO!

closer (x,y) (x',y') 	= if (x*x)+(y*y) >= (x'*x')+(y'*y') then False else True

toppoints xs 			= toppoints' xs []
toppoints' [] ys		= ys
toppoints' (x:xs) ys 	= toppoints' xs (take 100 (insert x ys))

insert x []				= [x]
insert x (y:ys)			= if closer x y then (x:y:ys) else y:(insert x ys)