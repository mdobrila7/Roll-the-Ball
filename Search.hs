{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import Data.Maybe

import ProblemState


data Node s a = Node
  { action :: (Maybe a)
  , state :: s
  , children :: [(Node s a)]
  , parent :: (Maybe (Node s a))
  , depth :: Int
  } deriving (Eq, Ord, Show)


nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeAction :: Node s a -> Maybe a
nodeAction = action

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

createNextNode :: (Eq s, Eq a, ProblemState s a) => a -> Node s a -> s -> Node s a
createNextNode nextAction prevNode nextState =
    let nextNode = (Node (Just nextAction) nextState (createChildren nextState nextNode) (Just prevNode) (nodeDepth prevNode + 1))
    in nextNode

createRoot :: (Eq s, Eq a, ProblemState s a) => s -> Node s a
createRoot nextState =
    let nextN = (Node Nothing nextState (createChildren nextState nextN) Nothing 1)
    in nextN

createChildren :: (Eq s, Eq a, ProblemState s a) => s -> Node s a -> [Node s a]
createChildren state parent = (map (\x -> (createNextNode (fst x) parent (snd x))) (successors state))

createStateSpace :: (ProblemState s a, Eq s, Eq a) => s -> Node s a
createStateSpace state = createRoot state

makeBfs :: Ord s => [([Node s a], [Node s a])] -> Node s a -> [([Node s a], [Node s a])]
makeBfs acc actualNode = if (isNothing (nodeParent actualNode))
    then (([actualNode], [actualNode]) : acc)
    else (makeBfs (acc ++ [([actualNode], [actualNode])]) actualNode)

bfs :: Ord s => Node s a -> [([Node s a], [Node s a])]
bfs= makeBfs []



bidirBFS :: Ord s => Node s a -> Node s a -> (Node s a, Node s a)
bidirBFS = undefined


extractPath ::(Eq s, Eq a) => Node s a -> [(Maybe a, s)]
extractPath actualNode = if (isNothing (nodeParent actualNode) == False)
    then (extractPath (fromJust (nodeParent actualNode))) ++ [(nodeAction actualNode, nodeState actualNode)]
    else [(Nothing, nodeState actualNode)]


solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește
      -> s          -- Starea finală la care se ajunge
      -> [(Maybe a, s)]   -- Lista perechilor
solve = undefined
