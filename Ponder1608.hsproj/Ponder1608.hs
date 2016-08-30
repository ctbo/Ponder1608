import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List
import Control.Monad
import Data.Maybe

type State = ([Int],IntSet)

initialState :: State
initialState = ([], S.empty)

n = 174

pick0to2 :: [a] -> [[a]]
pick0to2 = filter (\xs -> length xs <= 2) . subsequences

addNumber :: State -> [State]
addNumber (xs, forbidden) = concatMap f [start, start-1 .. 1]
  where start = if null xs then n else head xs - 1
        f x = if S.null $ S.intersection forbidden newSums
                then [(x:xs, S.union forbidden newSums)]
                else []
          where newSums = S.fromList $ map (sum . (x:)) $ pick0to2 xs
 
solutions = (iterate (>>= addNumber) [initialState]) !! 10

main = sequence_ $ map (print.fst) solutions

