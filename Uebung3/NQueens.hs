module NQueens where
import Util
import SearchProblem
import Data.List

data Move = Put Int deriving (Show, Eq)

nQueens :: Int -> SearchProblem [Int] Move
nQueens n = SP {
  initial    = [],
  successors = nQueensSuccessors n,
  isGoal     = \s -> length s == n,
  stepCost   = \s -> \a -> \s' -> 1,
  heuristic  = \s -> 1
}

nQueensSuccessors n s = if (length s >= n) then [] else
  [(Put a,s++[a]) | a <- [1..n], validMove s a]

-- check whether n is a valid move if the board is ss
validMove ss n
  | ss == []   =  True
  | elem n ss  =  False
  | otherwise  =  ss' == [s | (s,d) <- zip ss' [1..], s+d /= n, s-d /= n]
  where
    ss' = reverse ss


----------------------------------------------------------------------
-- functions for nicer output -- can be ignored for the moment
-- generate a dotty label for every board
nQueensS2N n s = "  "++ (show' s) ++
  " [style=rounded,shape=none,label=<<TABLE BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\">"++
  concat (map (nQueensS2N' n) (zip s' [1..])) ++ "</TABLE>>];"
  where s' = take n (s ++ repeat 0)

-- creates HTML code for one row of the board
-- n is the size of the board
-- e is the position of the queen (e==0, if there is no queen yet)
-- l is the number of the row (used for background color)
nQueensS2N' n (e,l) =
  "<TR>"++ (concat [ "<TD WIDTH=\"25\" HEIGHT=\"25\" FIXEDSIZE=\"TRUE\" "++
  -- set background color of tiles
  (if (l+i) `mod` 2 == 0 then "BGCOLOR=\"#EEEEEE\"" else "BGCOLOR=\"white\"")++
  -- make all empty lines gray
  (if e==0 then " COLOR=\"#AAAAAA\">" else ">")++
  -- place the queen on the board if i==e
  (if i==e then "&#9819;" else "")++
  "</TD>" | i <- [1..n] ]) ++"</TR>"
----------------------------------------------------------------------

----------------------------------------------------------------------
-- main function solving the nQueens problem through breadth first and depth first search
main = do
  let n = 7
  let sp = nQueens n
  putStrLn $ "NQueens for n=" ++ (show n)
  putStrLn $ "  bfs = " ++ (show (breadthFirstSearch sp))
  putStrLn $ "  dfs = " ++ (show (depthFirstSearch sp))
  -- putStrLn $ "  idfs = " ++ (show $ iterDepthFirstSearch sp)
  sp2dottyFile ((show n) ++ "queens.dot") sp (nQueensS2N n) 1000 >>= dot2pdf
