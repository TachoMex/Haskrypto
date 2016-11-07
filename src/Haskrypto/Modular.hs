module Haskrypto.Modular(
    Modular(..),
    inverse,
    power,
    extendedEuclidean,
    randomNumber,
    genRandomFermatPrime,
    fermatPrimalityTest,
    findFermatPrime,
    nextCoprime,
    gcd',
    squareRoot,
    modular,
    nextSafePrime,
    legendreSymbol,
    isSafeFermatPrime
)where
  import System.Random
  import Data.Bool
  import Debug.Trace
  import GHC.Exception
  import GHC.Stack
  import Data.List


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
      if m == n
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

  instance (Integral t, Ord t) =>  Fractional (Modular t) where
    a / x =  a * (inverse x)
    fromRational m = error "Can't create a modular from rational"

  inverse :: (Integral t) => Modular t -> Modular t
  inverse (Modular a n) =
    if gcd' == 1
      then
        (modular inverse_a n)
      else
        error $ "GCD is not 1"
      where
        (gcd', inverse_a, _) = extendedEuclidean a n

  extendedEuclidean :: (Integral a) => a -> a -> (a,a,a)
  extendedEuclidean a 0  = (a, 1, 0)
  extendedEuclidean a b  = (d, y, x - (a`div`b) * y) where
    (d, x, y) = extendedEuclidean b $ mod a b

  power :: (Integral a) => (Modular a) -> a -> (Modular a)
  power (Modular _ n) 0 = (Modular 1 n)
  power (Modular a n) 1 = (Modular a n)
  power a p = ap
    where
      ap = x * x * (power a (mod p 2))
      x  = power a $ div p 2

  fermatPrimalityTest ::  Integer -> Bool
  fermatPrimalityTest p = power (modular a p) p == modular a p
    where a = hcn_for_fermat_test

  genRandomFermatPrime n gen = findFermatPrime $ randomNumber n gen

  findFermatPrime n
    |  even n = findFermatPrime $ n + 1
    | otherwise =
      if fermatPrimalityTest n
        then
          n
        else
          findFermatPrime (n + 2)


  randomNumber ::  Int -> StdGen -> Integer
  randomNumber n gen = read $ take n $ randomRs ('0', '9') gen  :: Integer

  nextCoprime e n = if gcd' e n == 1 then e else nextCoprime (e + 1) n

  gcd' a 0 = a
  gcd' a b = gcd' b $ mod a b

  isSafeFermatPrime :: Integer -> Bool
  isSafeFermatPrime p = fermatPrimalityTest p && fermatPrimalityTest q
    where q = (p - 1) `div` 2

  nextSafePrime :: Integer -> Integer
  nextSafePrime a
    | a `mod` 2 == 0 = nextSafePrime' (a+1)
    | otherwise = nextSafePrime' a

  nextSafePrime' :: Integer -> Integer
  nextSafePrime' a
    | isSafeFermatPrime (2*a+1) = a
    | otherwise = nextSafePrime' (a+2) `debug` (show a)
      where debug = flip trace

  squareRoot :: Modular Integer -> Modular Integer
  squareRoot m@(Modular y p)
    | isSafeFermatPrime p = comprobate_sqrt root m
    | otherwise =  error "I don't know how to calculate it"
    where
      a = val $ inverse (Modular 2 q)
      q = (p - 1) `div` 2
      root = power m a

  legendreSymbol :: Integer -> Integer -> Integer
  legendreSymbol a p = if (val $ (Modular a p) ^ (div p 2)) == 1
                            then (-1)
                            else 1


  comprobate_sqrt y1 y2
    | y1 * y1 == y2 = y1
    | otherwise = error $ "Error while calculating squareRoot of " ++ show y1
