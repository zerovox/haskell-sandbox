{-# LANGUAGE RankNTypes #-}
-- From: http://blog.tmorris.net/posts/monad-exercises-in-scala/
data Monad' m = Monad' {
  unital :: forall a. a -> m a,
  flatMap :: forall a b. m a -> (a -> m b) -> m b
}

-- *** Monadic implementations ***

listMonad :: Monad' []
listMonad = Monad' {
	unital = (\x -> [x]),
	flatMap = (\x f -> concat $ map f x)
}

maybeMonad :: Monad' Maybe
maybeMonad = Monad' {
	unital = (\x -> Just x),
	flatMap = (\x f -> case x of
		Nothing -> Nothing
		Just x -> f x)
}

newtype Inter a = Inter { f :: Int -> a }
interMonad :: Monad' Inter
interMonad = Monad' {
	unital = (\x -> Inter {f = (\_ -> x)}),
	flatMap = (\x g -> Inter {f =(\y-> (f (g (f x y)) y))})
}

newtype Identity a = Identity { a :: a }
  deriving Show
identityMonad :: Monad' Identity
identityMonad = Monad' {
	unital = (\x -> Identity { a = x}),
	flatMap = (\x f -> f (a x))
}

-- *** Monadic functions ***

sequence' :: [m a] -> Monad' m -> m [a]
sequence' [] m = unital m []
sequence' (x:xs) m = flatMap m x (\y -> flatMap m (sequence' xs m) (\ys -> unital m (y:ys)))

fmap' :: m a -> (a -> b) -> Monad' m -> m b
fmap' o f m = flatMap m o (\z -> unital m (f z))

flatten :: m (m a) -> Monad' m -> m a
flatten o m = flatMap m o (\x -> x)

apply :: m (a -> b) -> m a -> Monad' m -> m b
apply m1 m2 m = flatMap m m1 (\f -> flatMap m m2 (\o -> unital m (f o)))

filterM' :: (a -> m Bool) -> [a] -> Monad' m -> m [a]
filterM' _ [] m = unital m []
filterM' p (a:as) m = flatMap m (p a) (\x -> flatMap m (filterM' p as m) (\xs -> unital m (if x then (a:xs) else xs)))

replicateM' :: Int -> m a -> Monad' m -> m [a]
replicateM' 0 _ m = unital m []
replicateM' n o m = flatMap m o (\a -> flatMap m (replicateM' (n-1) o m) (\as -> unital m (a:as)))

lift2 :: (a -> b -> c) -> m a -> m b -> Monad' m -> m c
lift2 f m1 m2 m = flatMap m m1 (\a -> flatMap m m2 (\b -> unital m (f a b)))