module Main where

import Data.Char ( ord, chr, isUpper )
import Control.Parallel ()
import Control.Parallel.Strategies ( parList, rdeepseq, using )
import Data.Time.Clock.POSIX

shiftedChar :: Char -> Int -> Char
shiftedChar c n =
  let base = if isUpper c then 'A' else 'a'
  in chr(mod ((ord c - ord base) + n) 26 + ord base)

shiftedWord :: [Char] -> Int -> [Char]
shiftedWord w n = fmap (`shiftedChar` n) w `using` parList rdeepseq

caeserify :: [Char] -> Int -> [Char]
caeserify m n = unwords (fmap (`shiftedWord` n) (words m))

main :: IO ()
main = do
  contents <- readFile "./prideAndPrejudice.txt"
  r1 <- (getPOSIXTime :: IO POSIXTime)
  print $ "start: " ++ (show r1)
  let result = caeserify contents 3
  r2 <- (getPOSIXTime :: IO POSIXTime)
  print $ "end: " ++ (show r2)
  print $ "time taken: " ++ (show $ r2 - r1)
  writeFile "./encodedPrideAndPrejudice" result
