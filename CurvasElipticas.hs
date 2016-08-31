module CurvasElipticas(
    Punto(..),
    CurvaElitica(..),
    bf_find_all,
    pertenece,
    suma,
    evalua,
    multiplica,
    negativo
  )where
  import Modular
  data CurvaElitica t = CurvaElitica {
    a :: t, 
    b :: t, 
    modulo :: t 
  }

  instance (Show t) => Show (CurvaElitica t) where
    show (CurvaElitica a b modulo) = 
      "y^2 = x^3 + "++ (show a) ++ "x + " ++ (show b) ++ " mod " ++ (show modulo) 

  data Punto t = Punto (t, t) | Infinity

  instance (Show t) => Show (Punto t) where
    show Infinity = "0"
    show (Punto p) = show p

  bf_find_all curva@(CurvaElitica a b modulo) = 
    filter (pertenece curva) pairs
    where 
      pairs = [Punto (x, y) | x <- [0..modulo-1], y <- [0..modulo-1]]

  pertenece (CurvaElitica a b modulo) (Punto (x,y)) = 
    y * y  `mod` modulo == (x * x * x + a * x + b) `mod` modulo 

  evalua :: (Integral t) => CurvaElitica t -> Modular t -> Punto t
  evalua (CurvaElitica a b modulo) x = Punto (x',y')
    where 
      y = square_root (x*x*x + (Modular a modulo)*x + (Modular b modulo))
      x' = valor x
      y' = valor y

  suma :: (Integral t) => CurvaElitica t -> Punto t -> Punto t -> Punto t
  suma _ Infinity a = a
  suma _ a Infinity = a
  suma (CurvaElitica a b p) (Punto (x1,y1)) (Punto (x2,y2)) 
    | fx1 == fx2 && (fy1 == (fix p (-fy2))) = Infinity
    | fx1 == fx2 && fy1 == fy2 = Punto (x3', y3')
    | otherwise = Punto (x3, y3)
    where
      m' = (3*x1*x1 + a) * ( valor $ inverso (Modular (2 * y1) (p)))
      x3' = fix p $ m' * m' - x1 - x2 
      y3' = fix p $ m'*(x1 - x3') - y1
      m = (y2 - y1) * inv_dif_x
      x3 = fix p $  m * m - x1 - x2
      y3 = fix p $ m * x3 - (y2*x1 - x2*y1) * inv_dif_x  
      inv_dif_x = valor $ inverso (Modular (x1 - x2) (p))
      fx1 = fix p x1
      fx2 = fix p x2
      fy1 = fix p y1
      fy2 = fix p y2

  multiplica :: (Integral t) => CurvaElitica t -> Punto t -> t -> Punto t
  multiplica e p 0 = Infinity 
  multiplica e p 1 = p
  multiplica e p n = t
    where 
      t = suma e x $ suma e x $ multiplica e p $ mod n 2
      x = multiplica e p $ n `div` 2

  negativo (Punto (x,y)) = Punto (x,-y)
{-
:l CurvasElipticas
let e = CurvaElitica 3 3 5
let p = Punto (3,2)
map (multiplica e p) [0..5]
-}

