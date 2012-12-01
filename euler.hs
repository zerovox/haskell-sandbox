-- A stupid attempt to solve project euler questions as haskell one liners. Absuses recurison by fixed points.

-- I'd rather not implement the sieve again, and this version(installed from cabal) is very efficient, it does make q7 a bit too easy though!
import Data.Numbers.Primes
import Data.List

-- converts an integer into a list of numbers
digits = fst . head . filter ((==) 0 . snd) . tail . iterate (\(d, n) -> let (q, r) = n `divMod` 10 in (r:d, q)) . (,) [] . abs

-- trusty y combinator, 
y f = f (y f)


q1 = sum $ filter (\x-> x `mod` 3 == 0|| x `mod` 5 == 0) [3..999]
q2 = sum $ takeWhile (<4000000) (filter (\x->x `mod` 2 == 0) (y (\f -> 0 : 1 : zipWith (+) f (tail f))))
q3 = y (\func n f -> if( f*f > n ) then n else if (n `mod` f == 0) then func (n `div` f) f else func n (f + 1)) 600851475143 2
q4 = maximum $ filter (\x -> (digits x ==) $ reverse $ digits x) [ i*j | i<-[100..999], j<-[100..999]]
q5 = head $ filter (\x-> (0 ==) $ sum $ map (x `mod`) [1..20]) [1..]
q7 = primes!!10001
q9 = filter (\[a,b,c]->a*a+b*b==c*c && a<b)[ [a,b,c] | a<-[1..1000], b<-[1..(1000-a)], c<-[1000-a-b]]
q10 = sum $ takeWhile (<2000000) primes
q25 = length $ takeWhile (\x -> length (digits x) < 1000) (y (\f -> 0 : 1 : zipWith (+) f (tail f)))
q29 = length $ nub [ a**b | a<-[2..100], b<-[2..100]]
q30 = sum $ filter (\x-> (sum (map (\y->y^5) (digits x))) == x) [10..354294]
q35 = length $ takeWhile (<1000000) primes
--Haskell takes a lot of the fun out of these hard numeric ones! Fairly easy to compute with modular exponetiation anyway, TODO!
q97 = (28433*2^7830457+1) `mod` 10^10

-- incomplete
q48 = (sum $ [x^x | x<-[1..1000]]) `mod` 10^10
