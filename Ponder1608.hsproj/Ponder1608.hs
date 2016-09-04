-- Solver for Ponder This August 2016
-- http://researchweb.watson.ibm.com/haifa/ponderthis/challenges/August2016.html
-- (C) 2016 by Harald Bögeholz
-- See LICENSE file for license information

import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List
import Control.Monad
import System.Environment

type State = ([Int],IntSet)

initialState :: State
initialState = ([], S.empty)

pick0to2 :: [a] -> [[a]]
pick0to2 = filter (\xs -> length xs <= 2) . subsequences

addNumber :: Int -> State -> [State]
addNumber n (xs, forbidden) = concatMap f [start, start-1 .. 1]
  where start = if null xs then n else head xs - 1
        f x = if any (`S.member` forbidden) newSums
                then []
                else [(x:xs, S.union forbidden (S.fromList newSums))]
          where newSums = map (sum . (x:)) $ pick0to2 xs
 
solutions n = (iterate (>>= addNumber n) [initialState]) !! 10

main = do
    args <- getArgs
    let n = case args of [] -> 174; [s] -> read s
    print (fst (head (solutions n)))

