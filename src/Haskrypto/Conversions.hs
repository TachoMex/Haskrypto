{-# LANGUAGE OverloadedStrings #-}
module Haskrypto.Conversions
where
  import qualified Data.ByteString.Lazy.Char8 as BL
  import Data.Char
  import GHC.Int
  import Numeric (showIntAtBase, showHex, readHex)

  hex n = showHex n ""

  password_generate :: GHC.Int.Int64 -> BL.ByteString -> Integer
  password_generate bytes message = unbincode aux_string
    where
      fixed_message = BL.take bytes $ BL.cycle message
      aux_string = reverse $ BL.unpack fixed_message

  unbincode s = unbincode' s 1 0

  unbincode' :: String -> Integer -> Integer -> Integer
  unbincode' [] _ acum = acum
  unbincode' (c:rest) weight acum = unbincode' rest weight' acum'
    where
      weight' =  weight * 256
      acum' = acum + (fromIntegral $ ord c) * weight

  bincode i = bincode' i []

  bincode' :: Integer -> String -> String
  bincode' 0 str = reverse str
  bincode' x str = bincode' remainder (c:str)
    where
      (remainder, current_char) = divMod x 256
      c = chr $ fromInteger current_char
