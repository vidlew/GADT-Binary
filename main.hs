module Main where

import GADTBinary
import System.Random

m = SI $ SI $ SO $ SI $ SI $ SI SB

main :: IO ()
main = do n <- getStdRandom (randomR (0,(elems m)-1))
          print $ toBinOrd n m

