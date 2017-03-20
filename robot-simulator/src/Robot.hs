module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

instance Enum Bearing where
  toEnum 0 = North
  toEnum 1 = East
  toEnum 2 = South
  toEnum 3 = West
  toEnum i = toEnum $ i `mod` 4

  fromEnum North = 0
  fromEnum East  = 1
  fromEnum South = 2
  fromEnum West  = 3

data Robot = Robot Bearing (Integer,Integer)

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ coord) = coord

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

advance :: Robot -> Robot
advance (Robot North (x, y)) = Robot North (x, y+1)
advance (Robot East  (x, y)) = Robot East  (x+1, y)
advance (Robot South (x, y)) = Robot South (x, y-1)
advance (Robot West  (x, y)) = Robot West  (x-1, y)

robotTurn :: (Bearing -> Bearing) -> Robot -> Robot
robotTurn turn (Robot bear coord) = Robot (turn bear) coord

robotLeft :: Robot -> Robot
robotLeft = robotTurn turnLeft

robotRight :: Robot -> Robot
robotRight = robotTurn turnRight

translateInstruction :: Char -> (Robot -> Robot)
translateInstruction 'A' = advance
translateInstruction 'L' = robotLeft
translateInstruction 'R' = robotRight

simulate :: Robot -> String -> Robot
simulate = foldl (\r i -> (translateInstruction i) r)

turnLeft :: Bearing -> Bearing
turnLeft = pred

turnRight :: Bearing -> Bearing
turnRight = succ
