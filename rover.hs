import Control.Monad (foldM)

type Pos = (Int, Int)
data Dir = North | East | South | West deriving (Show)
data Rover = Rover Dir Pos deriving (Show)

command :: Rover -> Char -> Rover
command (Rover dir pos) 'M'     = Rover dir (move dir pos)
command (Rover dir pos) turnDir = Rover (turn turnDir dir) pos

turn :: Char -> Dir -> Dir
turn 'L' dir = case dir of
  North -> West
  South -> East
  East  -> North
  West  -> South

turn 'R' dir = case dir of 
  North -> East
  South -> West
  East  -> South
  West  -> North

turn _ dir = dir

move :: Dir -> Pos -> Pos
move dir (x, y) = case dir of
  North -> (x, y+1)
  South -> (x, y-1)
  East  -> (x+1, y)
  West  -> (x-1, y)

rover :: String -> Rover
rover = foldl command (Rover North (0, 0))

roverM :: String -> Maybe Rover
roverM = foldM commandM (Rover North (0, 0))

commandM :: Rover -> Char -> Maybe Rover
commandM rover char = 
  if outOfBounds newRover then Nothing else Just newRover
  where newRover = command rover char

outOfBounds :: Rover -> Bool
outOfBounds (Rover _ (x,y)) = x > 10 || x < -10 || y > 10 || y < -10
