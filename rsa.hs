import Modular
import System.Random
import System.IO as IO
import Data.Char as DC
import Debug.Trace
import Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 as W8

desdeChunk :: BL.ByteString -> Integer -> Integer -> Integer
desdeChunk str i acum 
	|str == BL.empty = acum
	|otherwise = desdeChunk resto (i*256) (acum+i*(Prelude.toInteger  x))
		where
			x = BL.head str
			resto = BL.tail str

empaqueta :: Integer -> String -> String
empaqueta 0 acum = (DC.chr $ Prelude.length acum) : acum 
empaqueta n acum = empaqueta (div n 256) (DC.chr (Prelude.fromIntegral $ mod n 256) : acum)

--cifrar :: BL.ByteString -> Int64 -> Integer -> Integer -> Handle -> IO ()
cifrar texto tam_chunk n e arch 
	|texto == BL.empty = return ()
	|otherwise = do
	IO.hPutStr arch $ empaqueta c []
	cifrar resto tam_chunk n e arch
	where
		c = valor $ pow (Modular m n) e
		m = desdeChunk chunk 1 0
		chunk = BL.take tam_chunk texto
		resto = BL.drop tam_chunk texto
		

generarLlave = do 
		e' <- fmap (numero_aleatorio 200) newStdGen
		p <- fmap (primo_fermatt_aleatorio 100) newStdGen
		q <- fmap (primo_fermatt_aleatorio 100) newStdGen
		let	n = p * q
		let	phi_n = (p - 1) * (q - 1)
		let	e = siguiente_coprimo e' phi_n
		let	d = valor $ inverso (Modular e phi_n)
		return (n,e,d)
{-
opcionCifrado = do
	(n,e,d) <- generarLlave
	arch_sal <- IO.openFile "test.mp3.hid" WriteMode
	texto <- BL.readFile "test.mp3"
	cifrar texto 100 n e arch_sal
	hClose arch_sal
-}



main = do
