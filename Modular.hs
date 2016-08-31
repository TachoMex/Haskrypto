module Modular(
		Modular(..),
		inverso,
		pow,
		euclides_extendido,
		numero_aleatorio,
		primo_fermatt_aleatorio,
		fermatt_test_primalidad,
		busca_primo_fermatt,
		siguiente_coprimo,
		mcd,
		fix,
		square_root
)where
	import Debug.Trace
	import System.Random

	debug = flip trace

	data  Modular t = Modular{
		valor :: t,
		modulo :: t
	} 
	instance (Show t) => Show (Modular t) where
		show (Modular a n) = '(':(show a) ++ " mod "++(show n)++")" 

	instance (Eq t) => Eq (Modular t) where
		(Modular a _) == (Modular b _) = a==b 
		(Modular a _) /= (Modular b _) = a/=b 

	instance (Ord t) => Ord (Modular t) where
		(Modular a _) `compare` (Modular b _) = a `compare` b 

	instance (Integral t,Ord t) => Num (Modular t) where
		(Modular a n) + (Modular b m) = if m==n then (Modular ((a+b)`mod` n) n) else (Modular 0 0)
		(Modular a n) - (Modular b m) = if m==n then (Modular ((a+n-b)`mod` n) n) else (Modular 0 0)
		(Modular a n) * (Modular b m) = if m==n then (Modular ((a*b)`mod` n) n) else (Modular 0 0)

	instance (Integral t, Ord t) =>	Fractional (Modular t) where
		a / x =  a * (inverso x) 

	inverso :: (Integral t) => Modular t -> Modular t
	inverso (Modular a n) = if gcd == 1 then (Modular a' n) else (Modular 0 0)
		where 
			(gcd,a'',n') = euclides_extendido a n
			a' = mod (a'' + n) n 

	euclides_extendido :: (Integral a) => a -> a -> (a,a,a)
	euclides_extendido a 0  = (a,1,0)
	euclides_extendido a b  = (d,y,x-(a`div`b)*y) where
		(d,x,y) = euclides_extendido b $ mod a b 

	pow :: (Integral a) => (Modular a) -> a -> (Modular a)
	pow (Modular _ n) 0 = (Modular 1 n)
	pow (Modular a n) 1 = (Modular a n)
	pow a p = ap
		where
			ap = x * x * (pow a (mod p 2)) 
			x  = pow a $ div p 2

	fermatt_test_primalidad :: (Integral a) => a -> a -> Bool
	fermatt_test_primalidad a p = ((pow (Modular a p) p) == (Modular a p)) 

	busca_primo_fermatt :: (Integral a)=> a -> a
	busca_primo_fermatt n =  if  fermatt_test_primalidad 2 n then n else busca_primo_fermatt (n+1)

	primo_fermatt_aleatorio n gen = busca_primo_fermatt $ numero_aleatorio n gen 

	numero_aleatorio ::  Int -> StdGen -> Integer
	numero_aleatorio n gen = read $ take n $ randomRs ('0', '9') gen  ::Integer

	siguiente_coprimo e n = if mcd e n == 1 then e else siguiente_coprimo (e + 1) n

	mcd a 0 = a
	mcd a b = mcd b $ mod a b 

	fix :: Integral t => t -> t -> t
	fix n a = mod ((mod a n) + n) n

	square_root :: (Integral a) => (Modular a) -> (Modular a)
	square_root m@(Modular a p) 
		| mod p 4 == 3 = m ^ (div (p+1) 4)
		| otherwise = (Modular 0 0)
