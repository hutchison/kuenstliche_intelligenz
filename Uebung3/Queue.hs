module Queue (
  -- * Core Definition of a Queue
    Queue (..)
  -- * Simple instances of Queues
  , LifoQueue
  , FifoQueue
  , FifoQueue'
  -- * Advanced instances of Queues
  , TracingQueue (..)
  , mkTracingQueue
  , PriorityQueue (..)
  , showPriorityQueue
  , mkPriorityQueue
) where
import Data.List

-- | A queue is an abstract data type which supports
-- adding an element ('pop') and removing one element ('push')
-- In addition, it allows the construction of a new queue (via 'newQueue')
class Queue q where
  -- | Generates an empty queue
  newQueue :: q a
  -- | Returns 'True' if the queue is empty.
  empty  :: q a -> Bool
  -- | Pops one element from the queue, returning the element and the remaining queue
  pop  :: q a -> (a, q a)
  -- | Pushs a new element onto the queue
  push :: a -> q a -> q a
  -- | Extend the given queue with the set of elements
  extend :: [a] -> q a -> q a
  -- | Returns the current size of this queue
  size :: q a -> Int

data LifoQueue a = LIFO [a] deriving Show
instance Queue LifoQueue where
  newQueue             =  LIFO []
  empty (LIFO xs)      =  null xs
  pop (LIFO [])        =  error "Can't pop from an empty queue"
  pop (LIFO (x:xs))    =  (x, LIFO xs)
  push x (LIFO xs)     =  LIFO (x:xs)
  extend ys (LIFO xs)  =  LIFO (ys ++ xs)
  size (LIFO xs)       =  length xs

data FifoQueue a = FIFO [a] deriving Show
instance Queue FifoQueue where
  newQueue             =  FIFO []
  empty (FIFO xs)      =  null xs
  pop (FIFO [])        =  error "Can't pop from an empty queue"
  pop (FIFO (x:xs))    =  (x, FIFO xs)
  push x (FIFO xs)     =  FIFO (xs++[x])
  extend ys (FIFO xs)  =  FIFO (xs ++ ys)
  size (FIFO xs)       =  length xs

data FifoQueue' a = FIFO' [a] [a] deriving Show
instance Queue FifoQueue' where
  newQueue                 =  FIFO' [] []
  empty (FIFO' xs ys)      =  null xs && null ys
  pop (FIFO' [] [])        =  error "Can't pop from an empty queue"
  pop (FIFO' [] ys)        =  pop (FIFO' (reverse ys) [])
  pop (FIFO' (x:xs) ys)    =  (x, FIFO' xs ys)
  push y (FIFO' xs ys)     =  FIFO' xs (y:ys)
  extend yy (FIFO' xs ys)  =  FIFO' xs (ys++yy)
  size (FIFO' xs ys)       =  length xs + length ys

data PriorityQueue p a = PQ (a -> p) [(a,p)]
instance (Show a, Ord p, Show p) => Show (PriorityQueue p a) where
  show (PQ pf xs)  =  "PQ pf "++(show xs)
instance (Ord p) => Queue (PriorityQueue p) where
  newQueue                =  undefined
  empty (PQ pf xs)        =  null xs
  pop (PQ pf [])          =  error "Can't pop from an empty queue"
  pop (PQ pf ((x,_):xs))  =  (x, PQ pf xs)
  push x (PQ pf xs)       =  PQ pf (xs'++((x,px):xs''))
    where
      (xs',xs'')   =  span (\(x',px') -> px' < px) xs
      px           =  pf x
  extend ys q      =  foldr push q ys
  size (PQ pf xs)  =  length xs

showPriorityQueue :: ((a, p) -> String) -> PriorityQueue p a -> String
showPriorityQueue showF (PQ pf xs) = "PQ pf ["++(intercalate ", " $ map showF xs)++"]"

-- | 'mkPriorityQueue' creates an empty priority queue for the given priority function p
--
-- > *Queue> let pq = mkPriorityQueue (`mod` 10)
-- > *Queue> extend [0,4..30] pq
-- > PQ p [(0,0),(20,0),(12,2),(4,4),(24,4),(16,6),(8,8),(28,8)]
mkPriorityQueue :: (a -> p) -> PriorityQueue p a
mkPriorityQueue pf = PQ pf []

-- | A 'TracingQueue' traces its development over time.
-- It is derived from any other queue and it consists of the original underlying queue
-- and a trace of its history, which is a list of queues.
-- The history contains exactly those states in which a 'pop' was applied
-- Please note, the trace is in reverse order (for performance reasons).
-- A 'TracingQueue' cannot be constructed via 'newQueue' (which is 'undefined'),
-- but must be constructed via 'mkTracingQueue'.
--
-- > *Queue> let q = newQueue :: FifoQueue Int
-- > *Queue> q
-- > FIFO []
-- > *Queue> let q' = mkTracingQueue q
-- > *Queue> push 1 q'
-- > TQ {queue = FIFO [1], history = []}
-- > *Queue> pop $ push 3 $ snd $ pop $ push 2 $ push 1 q'
-- > (2,TQ {queue = FIFO [3], history = [FIFO [2,3],FIFO [1,2]]})
data TracingQueue q a = TQ { queue :: q a, history :: [q a]} deriving Show

instance (Queue q) => Queue (TracingQueue q) where
  newQueue = undefined
  empty (TQ q t) = empty q
  pop (TQ q t)   = let (x, q') = pop q in (x, TQ q' (q:t))
  push x (TQ q t) = let q' = push x q in TQ q' t
  extend xs (TQ q t) = let q' = extend xs q in TQ q' t
  size (TQ q t) = size q

mkTracingQueue q = TQ q []
