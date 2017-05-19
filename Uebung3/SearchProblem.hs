module SearchProblem where

import Util
import Data.List
import Data.Maybe
import Queue
import Debug.Trace

-- |Type of costs to evaluate solutions
type Cost = Double

-- |Data structure to define a search problem. It is parametrised by
-- the type 's' of states and the type 'a' of actions connecting them.
data SearchProblem s a = SP {
  -- | The initial state of the problem.
  initial :: s,
  -- | A set of direct successors reachable from a given state.
  -- The result consists of pairs of actions and resulting states.
  successors :: s -> [(a,s)],
  -- | Test whether the given state is a goal state.
  isGoal :: s -> Bool,
  -- | The cost of transition between the
  stepCost :: s -> a -> s -> Cost,
  -- | ...
  heuristic :: s -> Cost
}

-- one node of the search tree
type Node s a = (s, [a], Cost)

aStarT :: Eq s => SearchProblem s a -> (Maybe (Node s a), TracingQueue (PriorityQueue Cost) (Node s a))
aStarT sp = runTreeSearch' sp queue
  where
    queue = mkTracingQueue $ mkPriorityQueue (\(s,al,c) -> c + heuristic sp s)

aStar :: Eq s => SearchProblem s a -> (Maybe (Node s a), PriorityQueue Cost (Node s a))
aStar sp = runTreeSearch' sp queue
  where
    queue = mkPriorityQueue (\(s,al,c) -> c + heuristic sp s)

greedyT :: Eq s => SearchProblem s a -> (Maybe (Node s a), TracingQueue (PriorityQueue Cost) (Node s a))
greedyT sp = runTreeSearch' sp queue
  where
    queue = mkTracingQueue $ mkPriorityQueue (\(s,al,c) -> heuristic sp s)

greedy :: Eq s => SearchProblem s a -> (Maybe (Node s a), PriorityQueue Cost (Node s a))
greedy sp = runTreeSearch' sp queue
  where
    queue = mkPriorityQueue (\(s,al,c) -> heuristic sp s)

breadthFirstSearch :: (Eq s) => SearchProblem s a -> (Maybe (Node s a), FifoQueue (Node s a))
breadthFirstSearch sp = runTreeSearch' sp queue
  where
    queue = newQueue :: FifoQueue (Node s a)

depthFirstSearch :: (Eq s) => SearchProblem s a -> (Maybe (Node s a), LifoQueue (Node s a))
depthFirstSearch sp = runTreeSearch' sp queue
  where
    queue = newQueue :: LifoQueue (Node s a)

iterDepthFirstSearch :: Eq s => Int -> SearchProblem s a -> Maybe (Maybe (Node s a), LifoQueue (Node s a))
iterDepthFirstSearch maxDepth sp = head' $ filter (\s -> isJust $ fst s) $ [ runTreeSearch (\(s,al,c) -> length al < md) sp queue | md <- [0..maxDepth] ]
  where
    queue = newQueue :: LifoQueue (Node s a)


runTreeSearch :: (Queue q, Eq s) => (Node s a -> Bool) -> SearchProblem s a -> q (Node s a) -> (Maybe (Node s a), q (Node s a))
runTreeSearch expansionTest sp q = treeSearch expansionTest sp (push (initial sp, [], 0) q) []

runTreeSearch' :: (Queue q, Eq s) => SearchProblem s a -> q (Node s a) -> (Maybe (Node s a), q (Node s a))
runTreeSearch' = runTreeSearch (\n -> True)


treeSearch :: (Queue q, Eq s) => (Node s a -> Bool) -> SearchProblem s a -> q (Node s a) -> [s] -> (Maybe (Node s a), q (Node s a))
treeSearch expansionTest sp q vs
  | empty q       =  (Nothing, q)
  | isGoal sp s   =  (Just (s,al,c), q')
  | elem s vs     =  treeSearch expansionTest sp q' vs
  | otherwise     =  treeSearch expansionTest sp q'' vs'
  where
    -- get the first element from the queue
    (n@(s,al,c),q') = pop q
    -- extend the queue
    q'' = if expansionTest n then extend s' q' else q'
    -- compute the set of successor states
    s'  = [ (s',al++[a],c+(stepCost sp s a s')) | (a,s') <- successors sp s, not $ elem s' vs' ]
    -- add the current state s to the set of visited ones
    vs' = s:vs


-- computes the set of transitions starting from the initial state of the problem
-- this implements a breadth first search over
sp2graph :: (Eq s,Eq a) => SearchProblem s a -> Int -> [(s,a,s,Cost)]
sp2graph sp maxSteps = nub $ take maxSteps $ sp2graph' [initial sp] []
  where
    sp2graph' [] vs = []
    sp2graph' (s:ss) vs = [ (s,a,s',stepCost sp s a s') | (a,s') <- succs ] ++ sp2graph' ss' vs'
      where
        ss' = [ s' | (a,s') <- succs, not $ elem s' vs' ] ++ ss
        succs = (successors sp s)
        vs' = s:vs


-- computes the set of reachable states starting from the initial state of the problem
sp2states :: (Eq s,Eq a) => SearchProblem s a -> Int -> [s]
sp2states sp maxSteps = nub $ [s | (s,a,s',c) <- g] ++ [s' | (s,a,s',c) <- g]
  where g = sp2graph sp maxSteps


----------------------------------------------------------------------
-- functions for nicer output
-- creates a dotty-file for the given search problem, which displays the underlying tree
sp2dottyFile :: (Show s,Show a,Eq s,Eq a) => FilePath -> SearchProblem s a -> (s -> String) -> Int -> IO FilePath
sp2dottyFile fn sp s2n maxSteps = do
  writeFile fn (unlines dotData)
  putStrLn $ "% created dotty file in '"++fn++"'"
  return fn
  where
    i = initial sp
    dotData =
      [ "digraph G {" ] ++
      [ "  "++(show' s) ++" -> "++(show' s') ++ " [label=\""++(show'' a)++" ("++(show'' c)++")\"];" | (s,a,s',c) <- edges ] ++
      [ s2n s | s <- states ] ++
      [ "  "++(show' s) ++" [peripheries=2,color=green];" | s <- states, isGoal sp s ] ++
      [ "  INIT [shape=point,label=\"\"];  INIT -> "++(show' i)  ] ++
      [ "}" ]
    states = sp2states sp maxSteps
    edges = sp2graph sp maxSteps

sp2dottyFileCostOnly :: (Show s,Show a,Eq s,Eq a) => FilePath -> SearchProblem s a -> (s -> String) -> Int -> IO FilePath
sp2dottyFileCostOnly fn sp s2n maxSteps = do
  writeFile fn (unlines dotData)
  putStrLn $ "% created dotty file in '"++fn++"'"
  return fn
  where
    i = initial sp
    dotData =
      [ "digraph G {" ] ++
      [ "  "++(show' s) ++" -> "++(show' s') ++ " [label=\""++(show'' c)++"\"];" | (s,a,s',c) <- edges ] ++
      [ s2n s | s <- states ] ++
      [ "  "++(show' s) ++" [peripheries=2,color=green];" | s <- states, isGoal sp s ] ++
      [ "  INIT [shape=point,label=\"\"];  INIT -> "++(show' i)  ] ++
      [ "}" ]
    states = sp2states sp maxSteps
    edges = sp2graph sp maxSteps

defaultS2N :: Show a => a -> String
defaultS2N s = "  "++(show' s) ++" [shape=none,style=rounded,color=gray];"
