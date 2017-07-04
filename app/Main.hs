module Main where

import Data.List

main :: IO ()
main =
  readInt >>= \n ->
  readInts n >>= \nums ->
  putStrLn $ show $ pairwise nums

pairwise :: Integral a => [a] -> a
pairwise [] = 0
pairwise (_ : []) = 0
pairwise l = case (sortBy (flip compare) l) of
  (a : b : _) -> a * b

readInt :: (Integral a, Read a) => IO a
readInt = readLn

readInts :: (Integral a, Read a) => Int -> IO [a]
readInts n = fmap (take n . parseInts)  getLine

parseInts :: (Integral a, Read a) => String -> [a]
parseInts s =
  fmap read $ words s 
