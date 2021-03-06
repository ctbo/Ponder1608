-- Solver for Ponder This August 2016
-- http://researchweb.watson.ibm.com/haifa/ponderthis/challenges/August2016.html
-- (C) 2016 by Harald Bögeholz
-- See LICENSE file for license information

import System.Environment
import Data.Bits

type State3 = ([Int],(Integer,Integer,Integer))

initialState = ([],(0,0,0))

addNumber :: Int -> Int -> State3 -> [[Int]]
addNumber 0 _ (xs,_) = [xs]
addNumber depth maxN (xs, (forbidden1, forbidden2, forbidden3)) = concatMap f [start, start-1 .. depth]
  where start = if null xs then maxN else head xs - 1
        f x = if forbidden .&. newSums > 0
                then []
                else addNumber (depth-1) maxN ( x:xs, ( newSums1 .|. forbidden1
                                                      , newSums2 .|. forbidden2
                                                      , newSums3 .|. forbidden3
                                                      )
                                              )
          where newSums1 = bit x
                newSums2 = shiftL forbidden1 x
                newSums3 = shiftL forbidden2 x
                newSums = newSums1 .|. newSums2 .|. newSums3
                forbidden = forbidden1 .|. forbidden2 .|. forbidden3

solutions n = addNumber 10 n initialState

main = do
    args <- getArgs
    let n = case args of [] -> 174; [s] -> read s
    print (head (solutions n))

