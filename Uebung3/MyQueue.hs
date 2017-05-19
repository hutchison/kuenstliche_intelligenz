module Queue where

class Queue q where
    newQueue :: q a
    empty    :: q a -> Bool
    pop      :: q a -> (a, q a)
    push     :: a -> q a -> q a
    extend   :: [a] -> q a -> q a
    size     :: q a -> Int

data LifoQueue a = LIFO [a] deriving Show
instance Queue LifoQueue where
    newQueue            = LIFO []
    empty (LIFO xs)     = null xs
    pop (LIFO [])       = error "Can't pop from an empty queue."
    pop (LIFO (x:xs))   = (x, LIFO xs)
    push x (LIFO xs)    = LIFO (x:xs)
    extend ys (LIFO xs) = LIFO (ys ++ xs)
    size (LIFO xs)      = length xs

data FifoQueue a = FIFO [a] deriving Show
instance Queue FifoQueue where
    newQueue            = FIFO []
    empty (FIFO xs)     = null xs
    pop (FIFO [])       = error "Can't pop from an empty queue."
    pop (FIFO (x:xs))   = (x, FIFO xs)
    push x (FIFO xs)    = FIFO (xs ++ [x])
    extend ys (FIFO xs) = FIFO (xs ++ ys)
    size (FIFO xs)      = length xs


type Cost = Double

data SearchProblem s a = SP {
    initial :: s,
    successors :: s -> [(a, s)],
    isGoal :: s -> Bool,
    stepCost :: s -> a -> s -> Cost
}

breadthFirstSearch
    :: (Show a, Show s, Eq s)
    => SearchProblem s a -> Maybe (s, [a], Cost)
breadthFirstSearch sp = treeSearch sp queue []
    where
        equeue = newQueue :: FifoQueue (s, [a], Cost)
        queue = push (initial sp, [], 0) equeue

depthFirstSearch
    :: (Show a, Show s, Eq s)
    => SearchProblem s a -> Maybe (s, [a], Cost)
depthFirstSearch sp = treeSearch sp queue []
    where
        equeue = newQueue :: LifoQueue (s, [a], Cost)
        queue = push (initial sp, [], 0) equeue

treeSearch
    :: (Queue q, Eq s)
    => SearchProblem s a -> q (s, [a], Cost) -> [s] -> Maybe (s, [a], Cost)
treeSearch search_problem queue visited
    | empty queue                 = Nothing
    | isGoal search_problem state = Just (state, a1, c)
    | otherwise                   = treeSearch search_problem q'' vs'
    where
        -- get the first element from the queue
        ((state, a1, c), q') = pop queue
        -- extend the queue
        q'' = extend s' q'
        -- compute the set of successor states
        s' = [(s', a1 ++ [a], c + (stepCost search_problem state a s')) |
              (a, s') <- successors search_problem state, not $ elem s' vs']
        -- add the current state s to the set of visited ones
        vs' = state:visited


data City = A | B | C | D | E deriving (Show, Eq, Enum)

data Move = GoTo City deriving (Eq)
instance Show Move where show (GoTo c) = "→ " ++ (show c)


simple :: SearchProblem City Move
simple = SP {
    initial = A,
    successors = \current_city -> [(GoTo successor, successor)
                                   | (city, successor, _) <- simpleMap,
                                   city == current_city],
    isGoal = \s -> s == E,
    stepCost = simpleStepCost
}

simpleHeuristic = [(A, 7), (B, 2), (C, 3), (D, 1), (E, 0)]
simpleMap = [(A, B, 4), (A, C, 3), (B, E, 4), (C, D, 2), (D, E, 1)]

simpleStepCost :: City -> Move -> City -> Cost
simpleStepCost from (GoTo to) _ = head [cost | (f, t, cost) <- simpleMap, f == from, t == to]
