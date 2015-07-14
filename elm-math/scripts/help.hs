--A file to replace the second line of one file 
-- with the second line of another

module Main where

import Data.List.Split
import Data.List

main = do
  i <- readFile "test.old"
  let cssL = head $ tail $ splitOn "\n" i
  i2 <- readFile "test.html"
  let x = splitOn "\n" i2
      newF = [head x] ++ [cssL] ++ (tail $ tail x)
      f' = intersperse "\n" newF
  writeFile "test1.html" (concat f')  
