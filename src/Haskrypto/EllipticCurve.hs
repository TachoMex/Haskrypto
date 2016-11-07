module Haskrypto.EllipticCurve(
    Point(..),
    EllipticCurve(..),
    bfFindAll,
    belong,
    add,
    evaluate,
    evaluateSquare,
    multiply,
    negative,
  )where
  import Haskrypto.Modular

  data EllipticCurve t = EllipticCurve {
    a :: t,
    b :: t,
    field :: t
  }

  instance (Show t) => Show (EllipticCurve t) where
    show (EllipticCurve a b n) =
      "y^2 = x^3 + " ++
      show a ++
      "x + " ++
      show b ++
      " mod " ++
      show n

  data Point t = Point (t, t) | Infinity

  instance (Show t) => Show (Point t) where
    show Infinity = "0"
    show (Point p) = show p

  bfFindAll curve@(EllipticCurve a b n) =
    filter (belong curve) pairs
    where
      pairs = [Point (x, y) | x <- [0..n-1], y <- [0..n-1]]

  belong (EllipticCurve a b n) (Point (x,y)) =
    y * y  `mod` n == (x * x * x + a * x + b) `mod` n

  evaluate :: EllipticCurve Integer -> Integer -> Point Integer
  evaluate curve@(EllipticCurve a b n) v = Point (v, y')
    where
      x = modular v n
      y = squareRoot (x*x*x + (Modular a n)*x + (Modular b n))
      y' = val y

  evaluateSquare :: EllipticCurve Integer -> Integer -> Point Integer
  evaluateSquare curve@(EllipticCurve a b n) v = Point (v,val y)
    where
      x = modular v n
      y = squareRoot (x*x*x + (Modular a n)*x + (Modular b n))



  add :: (Integral t) => EllipticCurve t -> Point t -> Point t -> Point t
  add _ Infinity a = a
  add _ a Infinity = a
  add (EllipticCurve a b p) (Point (x1,y1)) (Point (x2,y2))
    | fx1 == fx2 && fy1 == (val $ modular (-fy2) p) = Infinity
    | fx1 == fx2 && fy1 == fy2 = Point (x3', y3')
    | otherwise = Point (x3, y3)
    where
      m' = (3 * x1 * x1 + a) * ( val $ inverse $ Modular (2 * y1) p)
      x3' = val $ modular (m' * m' - x1 - x2) p
      y3' = val $ modular (m'*(x1 - x3') - y1) p
      m = (y2 - y1) * inv_dif_x
      x3 = val $ modular (m * m - x1 - x2) p
      y3 = val $ modular (m * x3 - (y2 * x1 - x2 * y1) * inv_dif_x) p
      inv_dif_x = val $ inverse (Modular (x1 - x2) (p))
      fx1 = val $ modular x1 p
      fx2 = val $ modular x2 p
      fy1 = val $ modular y1 p
      fy2 = val $ modular y2 p

  multiply :: (Integral t) => EllipticCurve t -> Point t -> t -> Point t
  multiply e p 0 = Infinity
  multiply e p 1 = p
  multiply e p n = t
    where
      t = add e x $ add e x $ multiply e p $ mod n 2
      x = multiply e p $ n `div` 2

  negative (Point (x,y)) = Point (x,-y)
