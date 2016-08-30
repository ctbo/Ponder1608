import Data.IntSet (IntSet)
import qualified Data.IntSet as S
import Data.List
import Control.Monad
import System.Random
import Data.Maybe

type State = ([Int],IntSet)

initialState :: State
initialState = ([], S.empty)

n = 173

pick0to2 :: [a] -> [[a]]
pick0to2 = filter (\xs -> length xs <= 2) . subsequences

addNumber :: State -> [State]
addNumber (xs, forbidden) = concatMap f [start, start-1 .. 1]
  where start = if null xs then n else head xs - 1
        f x = if S.null $ S.intersection forbidden newSums
                then [(x:xs, S.union forbidden newSums)]
                else []
          where newSums = S.fromList $ map (sum . (x:)) $ pick0to2 xs
 
solutions = (iterate (>>= addNumber) [([],S.empty)]) !! 10

main = sequence_ $ map (print.fst) solutions

chooseRnd :: [a] -> StdGen -> (a, StdGen)
chooseRnd xs gen = (xs !! i, gen') 
  where (i, gen') = randomR (0, length xs - 1) gen
  
step :: Int -> State -> StdGen -> (Maybe State, StdGen)
step 0 state gen = (Just state, gen)
step n state gen = if null next
                      then (Nothing, gen)
                      else step (n-1) choice gen'
  where next = addNumber state
        (choice, gen') = chooseRnd next gen
{-        
main = do
  gen <- getStdGen
  work gen

work :: StdGen -> IO ()
work gen = do
  let (try, gen') = step 10 initialState gen
  when (isJust try) $ print try
  work gen'
  
-}  