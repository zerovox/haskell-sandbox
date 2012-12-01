--A state is a list of transition relations (pairs consisting of character read and successor state to transition to), and a boolean indicating if the state is an accepting state.
data State = State [(Char, State)] Bool deriving Show

-- Given a set of states xs, returns the next set of possible states after reading input character c
next c xs = concat (map (trans c) xs)

-- Given an input and a single state, returns the set of successor states
trans c (State xs b) = foldl (\ys (c',s) -> if c == c' then s:ys else ys) [] xs

-- Returns true if a current state is an accepting state
success []		= False
success ((State ys b):xs) = b || success xs

-- A depth first enumeration function, not great with cycles! If we add limits, we can get a iterative deepening search function. TODO!
dfenumerate (State ys b)
	| b = "":z
	| otherwise = z
	where z = concat (map (\(c, xs) -> prefix (dfenumerate xs) c) ys)

prefix xs c = map (\x -> c:x) xs

-- A breadth first enumeration function
enumerate s = enumerate'' [("",s)]
enumerate'' :: [(String, State)] -> [String]
enumerate'' xs = (foldl (\ys (pre, (State _ b)) ->  if b then pre:ys else ys) [] xs) ++ enumerate'' (nexts xs)

nexts :: [(String, State)] -> [(String, State)]
nexts [] = []
nexts ((pre, (State ys b)):xs) = (map (\(str, s) -> (pre ++ [str], s)) ys) ++ nexts xs


--A small example to show the difference between enumerate and enumerate'
--This NFA (DFA with alphabet {a,b}) accepts if the last character is a 'b'
s1 = State [('a', s1), ('b', s2)] False
s2 = State [('a', s1), ('b', s2)] True

--The same NFA, fewer transitions
s3 = State [('a', s3), ('b', s3), ('b', s4)] False
s4 = State [] True

-- *Main> take 10 $ enumerate s1
-- ["b","bb","ab","bbb","bab","abb","aab","bbbb","bbab","babb"]
-- *Main> take 10 $ enumerate s3
-- ["b","bb","ab","bbb","bab","abb","aab","bbbb","bbab","babb"]