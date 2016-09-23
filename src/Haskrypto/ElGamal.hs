module Haskrypto.ElGamal
where
  import Haskrypto.Modular hiding (field)
  import Haskrypto.ElipticCurve
  import System.Random

  data ElGamalKey = ElGamalKey{
    curve :: ElipticCurve Integer,
    p :: Point Integer,
    password :: Integer,
    public :: Point Integer
  } deriving (Show)

  construct_key :: (ElipticCurve Integer) -> (Point Integer) -> Integer -> ElGamalKey
  construct_key c p' pass = ElGamalKey {
    curve = c,
    p = p',
    password = pass,
    public = multiply c p' pass
  }

  crypt :: Integer -> ElGamalKey -> Point Integer -> Point Integer
  crypt message (ElGamalKey curve@(ElipticCurve _ _ primo) public_p sol_pass sol_public) tacho_public = coded
    where
      coded = add curve noise m_point
      m_point = evaluate curve (Modular message primo)
      noise = multiply curve tacho_public sol_pass

  decrypt :: Point Integer -> ElGamalKey -> Point Integer -> Point Integer
  decrypt coded (ElGamalKey curve@(ElipticCurve _ _ field) p pass _) public_key = message
    where
      message = add curve noise coded
      noise = negative $ multiply curve public_key pass


