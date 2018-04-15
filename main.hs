module Main where

import GADTBinary
import System.Random

m = SI $ SI $ SO $ SI $ SI $ SI SB

main :: IO ()
--main = do n <- getStdRandom (randomR (0,(elems m)-1))
--          print $ toBinOrd n m
--          putStrLn $ (reverse $ show $ toBinOrd n m) >>= \x -> if x `elem` "()" then [] else if x == ' ' then return '.' else return x
main = let l = (>>= \x -> if x `elem` "()" then "" else if x == ' ' then return '.' else return x) <$> show <$> binOrdList m in putStr $ unlines l
