module Main where

import Data.Char
import Control.Parallel
import Control.Parallel.Strategies

shiftedChar :: Char -> Int -> Char
shiftedChar c n = 
  let base = if isUpper c then 'A' else 'a'
  in chr((mod ((ord c - ord base) + n) 26) + ord base)

shiftedWord :: [Char] -> Int -> [Char]
shiftedWord w n = fmap (`shiftedChar` n) w `using` parList rdeepseq

caeserify :: [Char] -> Int -> [Char]
caeserify m n = unwords $ (fmap (`shiftedWord` n) (words m) `using` parList rdeepseq)

main :: IO ()
main = do
  contents <- readFile "./prideAndPrejudice.txt"
  writeFile "./encodedPrideAndPrejudice" $ caeserify contents 3
