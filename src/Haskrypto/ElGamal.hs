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

  crypt :: Point Integer -> ElGamalKey -> Point Integer -> Point Integer
  crypt m_point (ElGamalKey curve@(ElipticCurve _ _ primo) public_p password _) public_key = coded
    where
      coded = add curve noise m_point
      noise = multiply curve public_key password

  decrypt :: Point Integer -> ElGamalKey -> Point Integer -> Point Integer
  decrypt coded (ElGamalKey curve@(ElipticCurve _ _ field) p pass _) public_key = message
    where
      message = add curve noise coded
      noise = negative $ multiply curve public_key pass

  calculate_noise :: ElGamalKey -> Point Integer -> Point Integer
  calculate_noise (ElGamalKey curve _ password _) public_key = multiply curve public_key password
