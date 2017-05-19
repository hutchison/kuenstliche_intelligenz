module Simple where
import Util
import Data.List
import Data.Maybe
import Queue
import Search

data City = A | B | C | D | E deriving (Show,Eq,Enum)

data Move = GoTo City deriving (Eq)
instance Show Move where show (GoTo c) = "->"++(show c)

simple :: SearchProblem City Move
simple = SP {
  initial    = --define initial state
  successors = --define successor states
  isGoal     = --define goal condition
  stepCost   = --define step cost
  heuristic  = --define heuristic
}

simpleHeuristic = [ (A,7), (B,2), (C,3), (D,1), (E,0) ]

simpleMap = [ (A,B,4), (A,C,3), (B,E,4), (C,D,2), (D,E,1) ]

main = do
  putStrLn $ "Uninformed searches ..."
  let sp = simple

  showSearchResult  "  bfs    = " (breadthFirstSearch sp)
  showSearchResult  "  dfs    = " (depthFirstSearch sp)
  showSearchResult  "  idfs   = " (fromJust $ iterDepthFirstSearch 10 sp)

  putStrLn $ "\nOriginal heuristic ..."
  showSearchResult' "  greedy = " (greedyT sp)
  showSearchResult' "  a*     = " (aStarT sp)

  -- sp2dottyFileCostOnly "simple.dot" sp defaultS2N 1000 >>= dot2pdf

showSearchResult name (r, _) = do
  putStrLn $ name ++ (show r)

showSearchResult' name (r, TQ q t) = do
  putStrLn $ "\n"++name ++ (show r)
  let t' = reverse t
  let i = indent (length name)
  -- let shistory = map (showPriorityQueue (\((s,al,c),h) -> (show s)++"@"++(show c)++"~"++(show h))) t'
  let shistory = map (showPriorityQueue (\((s,al,c),h) -> (show s)++"/"++(show h))) t'
  mapM_ (\q -> putStrLn $ i ++ "- " ++ q) shistory
  putStrLn $ "           visited cities: " ++ (show $ nub [ p1o3 $ fst $ pop q | q <- t' ])
