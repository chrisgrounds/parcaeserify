module Main where

import Data.Char ( ord, chr, isUpper )
import Control.Parallel ()
import Control.Parallel.Strategies ( parList, rdeepseq, using )
import Data.Time.Clock.POSIX
import Data.Maybe
import Control.DeepSeq
import qualified Data.HashMap.Strict as HM

createLookupTable :: Int -> HM.HashMap Char Char
createLookupTable n =
  let lowercase = foldr (\next acc -> acc ++ [(next, shiftedChar next n)]) [] ['a'..'z']
      uppercase = foldr (\next acc -> acc ++ [(next, shiftedChar next n)]) [] ['A'..'Z']
   in HM.fromList $ lowercase ++ uppercase

shiftedChar :: Char -> Int -> Char
shiftedChar c n =
  let base = if isUpper c then 'A' else 'a'
  in chr(mod (ord c - ord base + n) 26 + ord base)

shiftedWord :: String -> HM.HashMap Char Char -> String
shiftedWord w lookupTable = fmap (\x -> fromMaybe '?' $ HM.lookup x lookupTable) w

caeserify :: String -> Int -> String
caeserify m n =
  let lookupTable = createLookupTable n
  in unwords (fmap (`shiftedWord` lookupTable) (words m))

main :: IO ()
main = do
  contents <- readFile "./prideAndPrejudice.txt"
  r1 <- (getPOSIXTime :: IO POSIXTime)
  print $ "start: " ++ show r1

  let result = caeserify contents 3
  deepseq result (return ())

  r2 <- (getPOSIXTime :: IO POSIXTime)
  print $ "end: " ++ show r2
  print $ "time taken: " ++ show (r2 - r1)
  writeFile "./encodedPrideAndPrejudice" result
