module Search where
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
  stepCost :: s -> a -> s -> Cost
}


breadthFirstSearch :: (Show a,Show s,Eq s) => SearchProblem s a -> Maybe (s, [a], Cost)
breadthFirstSearch sp = treeSearch sp queue []
  where
    --function "equeue" body
    queue = push (initial sp, [], 0) equeue


depthFirstSearch :: (Show a,Show s,Eq s) => SearchProblem s a -> Maybe (s, [a], Cost)
depthFirstSearch sp = treeSearch sp queue []
  where
    --function "equeue" body
    queue = push (initial sp, [], 0) equeue


treeSearch :: (Queue q, Eq s, Show s, Show a) => SearchProblem s a -> q (s, [a], Cost) -> [s] -> Maybe (s, [a], Cost)
treeSearch sp q vs
  | empty q       =  Nothing
  | isGoal sp s   =  Just (s,al,c)
  | otherwise     =  trace ("searching" ++ show vs') treeSearch sp q'' vs'
  where
    -- get the first element from the queue
    ((s,al,c),q') = pop q
    -- extend the queue
    q'' = extend s' q'
    -- compute the set of successor states
    s'  = [ (s',al++[a],c+(stepCost sp s a s')) | (a,s') <- successors sp s, not $ elem s' vs' ]
    -- add the current state s to the set of visited ones
    vs' = s:vs
