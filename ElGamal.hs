module ElGamal(
  gen_p_form_4k_3
) where
  import Modular
  import CurvasElipticas
  import System.Random

  gen_p_form_4k_3 = do
    p <- fmap (primo_fermatt_aleatorio 100) newStdGen
    if 
      mod p 4 == 3
    then 
      return p
    else
      gen_p_form_4k_3

  primo = 2312066423842784199335084501408249682709570295350497126202262694916832160006563584489701247125840703

  curva = CurvaElitica 1721187123014121499175528630365445314703481949273336323103603777500510713045970073798535551733653022 782100911881054109868590569204230085709698383857991686836638030260343714596088060898873315677798446 primo

  mensaje = evalua curva (Modular 1654078046889374115826048593768579557849507835041106780479 primo)

  public_p = Punto (1898873451654960804976047127349585056505542854552412860104777685425735218754483243475743252062399490,1935313504578292109772439114864808814316506192563492508710368070077794171135502257077957399482940457)
  secret_x = 636101865554538397279068212560259305319108095764776131022429006457200388392425748389387630432763672

  public_y = multiplica curva public_p secret_x
  secret'_k = 63477708164497919135989144307676463779426901320686271130229911868611444156682721732150291254417180
  c1 = multiplica curva public_p secret'_k
  c2 = multiplica curva public_y secret'_k

  cifrado = (c1, suma curva c2 mensaje)

-- (c,d) = cifrado
-- c' = multiplica curva c secret_x
-- m' = suma curva d $ negativo c'
