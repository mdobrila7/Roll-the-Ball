{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}
module RollTheBall where
import Pipes
import ProblemState

import qualified Data.Array as A



data Directions = North | South | West | East | Other
    deriving (Show, Eq, Ord)


type Position = (Int, Int)


data Cell = Cell
  { vtype :: Char
  , position :: Position
} deriving (Eq, Ord)



instance Show Cell where
    show (Cell c pos) = [c]


data Level = Level {

    -- o matrice indexata dupa pozitii, care are ca elemente celulele
    myMap :: A.Array Position Cell

    -- starea jocului
    {-state :: State-}

} deriving (Eq, Ord)
               

instance Show Level where
    show level = (foldl (\acc x -> if (snd (fst x)) == snd (snd (A.bounds (myMap level)))
                                    then acc ++ (show (snd x)) ++ "\n" 
                                    else acc ++ (show (snd x))  ) "\n" (A.assocs (myMap level))) ++ ""


emptyLevel :: Position -> Level
emptyLevel (n, m) = Level ((A.array ((0, 0), (n,m)) [((x, y), (Cell emptySpace (x, y))) | x <- [0..n], y <- [0..m]]))
                               


addCell :: (Char, Position) -> Level -> Level
addCell (ch, pos) (Level matrix) =  if (fst (snd (A.bounds matrix)) >= fst (pos)) && (snd (snd (A.bounds matrix)) >= snd (pos)) && (fst (fst (A.bounds matrix)) <= fst (pos)) && (snd (fst (A.bounds matrix)) <= snd (pos)) && (matrix A.! pos) == (Cell emptySpace pos)
                                        then Level (matrix A.// [(pos, Cell ch pos)])
                                        else Level matrix

 
createLevel :: Position -> [(Char, Position)] -> Level
createLevel pos l = foldr (\el acc -> addCell el acc) (emptyLevel (fst(pos),snd(pos))) l
             


moveCell :: Position -> Directions -> Level -> Level


moveCell (n, m) dir (Level matrix) = Level (if (fst (snd (A.bounds matrix)) >= n) && (snd (snd (A.bounds matrix)) >= m) && (fst (fst (A.bounds matrix)) <= n) && (snd (fst (A.bounds matrix)) <= m)
                                                
                                                then (if (vtype (matrix A.! (n, m)) /= startUp && vtype (matrix A.! (n, m)) /= startDown && vtype (matrix A.! (n, m)) /= startLeft && vtype (matrix A.! (n, m)) /= startRight &&
                                                    vtype (matrix A.! (n, m)) /= winUp && vtype (matrix A.! (n, m)) /= winDown && vtype (matrix A.! (n, m)) /= winLeft && vtype (matrix A.! (n, m)) /= winRight)
                                                    
                                                    then (if (dir == North) && (n > 0) && (vtype (matrix A.! (n-1,m)) == emptySpace)
                                                        then (matrix A.// [((n-1,m),  Cell (vtype (matrix A.! (n, m))) (n-1, m)), ((n,m), (Cell emptySpace (n,m)))])

                                                     else if (dir == South) && (n < fst (snd (A.bounds matrix))) && (vtype (matrix A.! (n+1, m)) == emptySpace)
                                                        then (matrix A.// [((n+1, m), Cell (vtype (matrix A.! (n, m))) (n+1, m)), ((n, m), (Cell emptySpace (n,m)))])

                                                     else if (dir == East) && (m < snd (snd (A.bounds matrix))) && (vtype (matrix A.! (n, m+1)) == emptySpace)
                                                        then (matrix A.// [((n, m+1), Cell (vtype (matrix A.! (n, m))) (n, m+1)), ((n,m), (Cell emptySpace (n,m)))])

                                                     else if (dir == West) && (m > 0) && (vtype (matrix A.! (n, m-1)) == emptySpace)
                                                        then (matrix A.// [((n, m-1),  Cell (vtype (matrix A.! (n, m))) (n, m-1)), ((n,m), (Cell emptySpace (n,m)))])

                                                     else matrix)

                                                else matrix)

                                            else matrix)
                                                 
                                     

connection :: Cell -> Cell -> Directions -> Bool
connection (Cell a _) (Cell b _) dir =  if (a == botRight) && (b == horPipe) && (dir == West)
                                        then True
                                    else if (a == botRight) && (b == verPipe) && (dir == North) 
                                        then True
                                    else if (a == botRight) && (b == topLeft) && (dir == North)
                                        then True
                                    else if (a == botRight) && (b == topRight) && (dir == North)
                                        then True
                                    else if (a == botRight) && (b == topLeft) && (dir == West)
                                        then True
                                    else if (a == botRight) && (b == botLeft) && (dir == West)
                                        then True


                                    else if (a == botLeft) && (b == verPipe) && (dir == North)
                                        then True
                                    else if (a == botLeft) && (b == topLeft) && (dir == North)
                                        then True
                                    else if (a == botLeft) && (b == topRight) && (dir == North)
                                        then True
                                    else if (a == botLeft) && (b == horPipe) && (dir == East)
                                        then True
                                    else if (a == botLeft) && (b == botRight) && (dir == East)
                                        then True
                                    else if (a == botLeft) && (b == topRight) && (dir == East)
                                        then True


                                    else if (a == topLeft) && (b == verPipe) && (dir == South) 
                                        then True
                                    else if (a == topLeft) && (b == horPipe) && (dir == East) 
                                        then True
                                    else if (a == topLeft) && (b == botLeft) && (dir == South)
                                        then True
                                    else if (a == topLeft) && (b == botRight) && (dir == South)
                                        then True
                                    else if (a == topLeft) && (b == botRight) && (dir == East)
                                        then True
                                    else if (a == topLeft) && (b == topRight) && (dir == East)
                                        then True


                                    else if (a == topRight) && (b == verPipe) && (dir == South) 
                                        then True
                                    else if (a == topRight) && (b == horPipe) && (dir == West) 
                                        then True
                                    else if (a == topRight) && (b == botLeft) && (dir == South)
                                        then True
                                    else if (a == topRight) && (b == botRight) && (dir == South)
                                        then True
                                    else if (a == topRight) && (b == topLeft) && (dir == West)
                                        then True
                                    else if (a == topRight) && (b == botLeft) && (dir == West)
                                        then True


                                    else if (a == verPipe) && (b == verPipe) && (dir == North) 
                                        then True
                                    else if (a == verPipe) && (b == verPipe) && (dir == South) 
                                        then True
                                    else if (a == verPipe) && (b == topLeft) && (dir == North) 
                                        then True
                                    else if (a == verPipe) && (b == topRight) && (dir == North) 
                                        then True
                                    else if (a == verPipe) && (b == botLeft) && (dir == South) 
                                        then True
                                    else if (a == verPipe) && (b == botRight) && (dir == South) 
                                        then True


                                    else if (a == horPipe) && (b == horPipe) && (dir == West) 
                                        then True
                                    else if (a == horPipe) && (b == horPipe) && (dir == East) 
                                        then True
                                    else if (a == horPipe) && (b == topLeft) && (dir == West) 
                                        then True
                                    else if (a == horPipe) && (b == topRight) && (dir == East) 
                                        then True
                                    else if (a == horPipe) && (b == botLeft) && (dir == West) 
                                        then True
                                    else if (a == horPipe) && (b == botRight) && (dir == East) 
                                        then True


                                    else if (a == startUp) && (b == verPipe) && (dir == North)
                                        then True
                                    else if (a == startUp) && (b == topLeft) && (dir == North)
                                        then True
                                    else if (a == startUp) && (b == topRight) && (dir == North)
                                        then True
                                    else if (a == startUp) && (b == winDown) && (dir == North)
                                        then True


                                    else if (a == startDown) && (b == verPipe) && (dir == South)
                                        then True
                                    else if (a == startDown) && (b == botLeft) && (dir == South)
                                        then True
                                    else if (a == startDown) && (b == botRight) && (dir == South)
                                        then True
                                    else if (a == startDown) && (b == winUp) && (dir == South)
                                        then True


                                    else if (a == startLeft) && (b == horPipe) && (dir == West)
                                        then True
                                    else if (a == startLeft) && (b == topLeft) && (dir == West)
                                        then True
                                    else if (a == startLeft) && (b == botLeft) && (dir == West)
                                        then True
                                    else if (a == startLeft) && (b == winRight) && (dir == West)
                                        then True


                                    else if (a == startRight) && (b == horPipe) && (dir == East)
                                        then True
                                    else if (a == startRight) && (b == topRight) && (dir == East)
                                        then True
                                    else if (a == startRight) && (b == botRight) && (dir == East)
                                        then True
                                    else if (a == startRight) && (b == winLeft) && (dir == East)
                                        then True


                                    else if (a == verPipe) && (b == winUp) && (dir == South)
                                        then True
                                    else if (a == topLeft) && (b == winUp) && (dir == South)
                                        then True
                                    else if (a == topRight) && (b == winUp) && (dir == South)
                                        then True



                                    else if (a == verPipe) && (b == winDown) && (dir == North)
                                        then True
                                    else if (a == botLeft) && (b == winDown) && (dir == North)
                                        then True
                                    else if (a == botRight) && (b == winDown) && (dir == North)
                                        then True


                                    else if (a == horPipe) && (b == winLeft) && (dir == East)
                                        then True
                                    else if (a == topLeft) && (b == winLeft) && (dir == East)
                                        then True
                                    else if (a == botLeft) && (b == winLeft) && (dir == East)
                                        then True


                                    else if (a == horPipe) && (b == winRight) && (dir == West)
                                        then True
                                    else if (a == topRight) && (b == winRight) && (dir == West)
                                        then True
                                    else if (a == botRight) && (b == winRight) && (dir == West)
                                        then True


                                    else False




retpos :: Level -> Position
retpos = undefined

searchInit :: [Cell] -> Cell
searchInit matrix =
    if ((vtype (head matrix)) == startRight || (vtype (head matrix)) == startLeft || (vtype (head matrix)) == startUp || (vtype (head matrix)) == startDown)
        then (head matrix)
        else (searchInit (tail matrix))

searchFinish :: [Cell] -> Cell
searchFinish matrix =
    if ((vtype (head matrix)) == winRight || (vtype (head matrix)) == winLeft || (vtype (head matrix)) == winUp || (vtype (head matrix)) == winDown) 
        then (head matrix)
        else (searchFinish (tail matrix))

find :: Directions -> (A.Array (Int, Int) Cell) -> Cell ->Bool
find direction matrix (Cell vtype (lin, col)) =
    if (vtype == winLeft || vtype == winRight || vtype == winUp || vtype == winDown)
        then True
        else
    if (lin < fst (snd (A.bounds matrix)) && (connection (Cell vtype (lin, col)) (matrix A.! (lin + 1, col)) South == True && (direction == North || direction == West || direction == East || direction == Other)))
        then (find North matrix (matrix A.! (lin + 1, col)))
        else
    if (lin > 0 && (connection (Cell vtype (lin, col)) (matrix A.! (lin - 1, col)) North == True && (direction == South || direction == West || direction == East || direction == Other)))
        then (find South matrix (matrix A.! (lin - 1, col)))
        else
    if (col < snd (snd (A.bounds matrix)) && (connection (Cell vtype (lin, col)) (matrix A.! (lin, col + 1)) East == True && (direction == North || direction == West || direction == South || direction == Other)))
        then (find West matrix (matrix A.! (lin, col + 1)))
        else
    if (col > 0 && (connection (Cell vtype (lin, col)) (matrix A.! (lin, col - 1)) West == True && (direction == North || direction == South || direction == East || direction == Other)))
        then (find East matrix (matrix A.! (lin, col - 1)))
        else False


wonLevel :: Level -> Bool
wonLevel (Level matrix) = find Other matrix (searchInit (A.elems matrix))

checkChar :: Char -> Bool
checkChar cellType = ((cellType `notElem` startCells) && (cellType `notElem` winningCells) && (cellType /= emptySpace))

swap :: Position -> Position -> Level -> Level
swap pos1 pos2 (Level matrix) = (Level newM)
    where src = matrix A.! pos1
          dst = matrix A.! pos2
          newM = matrix A.// [(pos1, (Cell (vtype dst) pos1)), (pos2, (Cell (vtype src) pos2))]

getSucc :: Level -> Cell -> [((Position, Directions), Level)] -> [((Position, Directions), Level)]
getSucc (Level matrix) (Cell temptype (lin, col)) res = res ++ (
        if ((checkChar temptype)
        && col /= snd (snd (A.bounds matrix))
        && (vtype (matrix A.! (lin, col + 1))) == emptySpace)
        then [(((lin, col), East), swap (lin, col) (lin, col + 1) (Level matrix))]
        else [])
        ++ (if ((checkChar temptype)
        && col /= 0
        && (vtype (matrix A.! (lin, col - 1))) == emptySpace)
        then [(((lin, col), West), swap (lin, col) (lin, col - 1) (Level matrix))]
        else [])
        ++ (if ((checkChar temptype)
        && lin /= fst (snd (A.bounds matrix))
        && (vtype (matrix A.! (lin + 1, col))) == emptySpace)
        then [(((lin, col), South), swap (lin, col) (lin + 1, col) (Level matrix))]
        else [])
        ++ (if ((checkChar temptype)
        && lin /= 0
        && (vtype (matrix A.! (lin - 1, col))) == emptySpace)
        then [(((lin, col), North), swap (lin, col) (lin - 1, col) (Level matrix))]
        else [])

instance ProblemState Level (Position, Directions) where
    successors (Level cells) = foldr (\cell res -> (getSucc (Level cells) cell res)) [] (A.elems cells)

    isGoal = undefined
     {-level
        | (wonLevel level) = True
        | otherwise = False
        -}

    reverseAction = undefined
