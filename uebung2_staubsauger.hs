module Agent where

import Prelude hiding (Left, Right)

data Action   = Left | Right | Suck | Wait deriving (Show)
data Location = A | B deriving (Show, Eq)
data Status   = Dirty | Clean deriving (Show)

type Percept       = (Location, Status)
type WorldState    = ([(Location, Status)], Location)
type AgentFunction = Percept -> Action
type Behaviour     = [Percept] -> [Action]
type Environment   = [Action] -> [Percept]
type ActionEffect  = Action -> WorldState -> WorldState
type Observer      = WorldState -> Percept


vac :: AgentFunction
vac (_, Dirty) = Suck
vac (A, Clean) = Right
vac (B, Clean) = Left


initS :: WorldState
initS = ([(A, Dirty), (B, Dirty)], A)


suck_dirt :: Location -> (Location, Status) -> (Location, Status)
suck_dirt curr_location (location, Dirty) = if curr_location == location
                                            then (location, Clean)
                                            else (location, Dirty)
suck_dirt _ (location, Clean) = (location, Clean)

eff :: ActionEffect
eff Left (states, _)             = (states, A)
eff Right (states, _)            = (states, B)
eff Wait a                       = a
eff Suck (states, curr_location) = (map (suck_dirt curr_location) states, curr_location)


obsEv :: Observer
obsEv ([(A, state_a), (B, state_b)], A) = (A, state_a)
obsEv ([(A, state_a), (B, state_b)], B) = (B, state_b)
obsEv ([(B, state_b), (A, state_a)], A) = (A, state_a)
obsEv ([(B, state_b), (A, state_a)], B) = (B, state_b)


obs :: [Percept]
obs = [(A, Dirty), (A, Clean), (B, Dirty)]


act :: AgentFunction -> Behaviour
-- act af ps = map af ps
act = map


affect :: ActionEffect -> [Action] -> WorldState -> [WorldState]
affect eff []     world_state = []
affect eff (a:as) world_state = let
        ws' = (eff a world_state)
    in
        [ws'] ++ affect eff as ws'


env :: ActionEffect -> Observer -> WorldState -> Environment
{-
env ::  (Action -> WorldState -> WorldState)
        -> (WorldState -> Percept)
        -> WorldState
        -> [Action]
        -> [Percept]
-}
-- env eff obsEv world_state actions = map obsEv (affect eff actions initS)
env e o w (a:as) =
    let
        w' = e a w
    in
        o w' : env e o w' as
