module Main where
  import Haskrypto.Modular
  import System.Exit
  import Control.Concurrent
  import Control.Concurrent.MVar


  prime_finder v x = do
    print x
    if is_safe_fermat_prime (x+x+1)
      then do
        putMVar v x
      else do
        prime_finder v (x+10)

  seed = 8881881231233123123^100
  main = do
    v <- newEmptyMVar
    h1 <- forkOS $ prime_finder v (seed+1)
    h2 <- forkOS $ prime_finder v (seed+3)
    h3 <- forkOS $ prime_finder v (seed+7)
    h4 <- forkOS $ prime_finder v (seed+9)
    x <- takeMVar v

    killThread h1
    killThread h2
    killThread h3
    killThread h4

    putStrLn "The prime is:"
    print x