module Main where

import GADTBinary
import System.Random

--Print the numbers from 0 to m-1
m = SO $ SO $ SI $ SO $ SI $ SI $ SI SB

main :: IO ()
--main = do n <- getStdRandom (randomR (0,(elems m)-1))
--          print $ toBinOrd n m
--          putStrLn $ (reverse $ show $ toBinOrd n m) >>= \x -> if x `elem` "()" then [] else if x == ' ' then return '.' else return x
--main = let l = reverse <$> (>>= \x -> if x `elem` "()" then "" else if x == ' ' then return '.' else return x) <$> show <$> binOrdList m in putStr $ unlines l
main = let l = reverse <$> (>>= \x -> if x `elem` "( C)" then ""  else if x `elem` "RG" then "1" else if x=='L' then "0" else "2") <$> show <$> binOrdList m in putStr $ "Numbers from 0 up to (but not including) " ++ (show $ elems m) ++ " in binary:\n" ++ unlines l
