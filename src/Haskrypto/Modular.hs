module Haskrypto.Modular(
		Modular(..),
		inverse,
		power,
		extended_euclidean,
		random_number,
		gen_random_fermat_prime,
		fermat_primality_test,
		find_fermat_prime,
		next_coprime,
		gcd',
		square_root,
		modular
)where
	import System.Random
	import Data.Bool
	-- HCN -> Highly Composite Number
	hcn_for_fermat_test = 720720

	data  Modular t = Modular{
		val :: t,
		field :: t
	}

	modular :: (Integral t) => t -> t -> Modular t
	modular v' m = Modular v m
		where
			v = mod v' m

	instance (Show t) => Show (Modular t) where
		show (Modular a n) = "(" ++ show a ++ " mod " ++ show n ++")"

	instance (Eq t) => Eq (Modular t) where
		(Modular a _) == (Modular b _) = a == b
		(Modular a _) /= (Modular b _) = a /= b

	instance (Ord t) => Ord (Modular t) where
		(Modular a _) `compare` (Modular b _) = a `compare` b

	instance (Integral t, Ord t) => Num (Modular t) where
		(Modular a n) + (Modular b m) =
			if m==n
				then
					(modular ((a + b) `mod` n) n)
				else
					error "Operating numbers with diferent modulus"
		(Modular a n) - (Modular b m) =
			if m==n
				then
					(modular ((a + n- b)`mod` n) n)
				else
					error "Operating numbers with diferent modulus"
		(Modular a n) * (Modular b m) =
			if m==n
				then
					(modular ((a * b)`mod` n) n)
				else
					error "Operating numbers with diferent modulus"
		abs m = m
		signum m = 1

	instance (Integral t, Ord t) =>	Fractional (Modular t) where
		a / x =  a * (inverse x)
		fromRational m = error "Can't create a modular from rational"

	inverse :: (Integral t) => Modular t -> Modular t
	inverse (Modular a n) =
		if gcd' == 1
			then
				(modular inverse_a n)
			else
				error "GCD is not 1"
			where
				(gcd', inverse_a, _) = extended_euclidean a n

	extended_euclidean :: (Integral a) => a -> a -> (a,a,a)
	extended_euclidean a 0  = (a, 1, 0)
	extended_euclidean a b  = (d, y, x - (a`div`b) * y) where
		(d, x, y) = extended_euclidean b $ mod a b

	power :: (Integral a) => (Modular a) -> a -> (Modular a)
	power (Modular _ n) 0 = (Modular 1 n)
	power (Modular a n) 1 = (Modular a n)
	power a p = ap
		where
			ap = x * x * (power a (mod p 2))
			x  = power a $ div p 2

	fermat_primality_test :: (Integral a) => a -> Bool
	fermat_primality_test p = power (modular a p) p == modular a p
		where a = hcn_for_fermat_test

	gen_random_fermat_prime n gen = find_fermat_prime $ random_number n gen

	find_fermat_prime n
		|	even n = find_fermat_prime $ n + 1
		| otherwise =
			if fermat_primality_test n
				then
					n
				else
					find_fermat_prime (n + 2)


	random_number ::  Int -> StdGen -> Integer
	random_number n gen = read $ take n $ randomRs ('0', '9') gen  :: Integer

	next_coprime e n = if gcd' e n == 1 then e else next_coprime (e + 1) n

	gcd' a 0 = a
	gcd' a b = gcd' b $ mod a b

	is_safe_fermat_prime :: (Integral a) => a -> Bool
	is_safe_fermat_prime p = fermat_primality_test p && fermat_primality_test q
		where q = (p - 1) `div` 2

	square_root :: (Integral a) => (Modular a) -> (Modular a)
	square_root m@(Modular y p)
		| is_safe_fermat_prime p = root
		| otherwise =  error "I don't know how to calculate it"
		where
			a = val $ inverse (Modular 2 q)
			q = (p - 1) `div` 2
			sr = power m a
			v = val sr
			root = if 2 * v > p
				then
					modular (-v) p
				else
					sr
