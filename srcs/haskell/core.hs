import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  let state = parseState args
      nextState = update state
  putStrLn $ printState nextState
  putStrLn $ render nextState

data Move = Down | Left | Right | Rotate_C | Rotate_C_C | Drop deriving (Enum)
instance Show Move where
  show = show . fromEnum

type Board = String
data State = State
             {
               move :: Move,
               board :: Board,
               figIndex :: Int,
               rotateIndex :: Int,
               color :: Int,
               offsetX :: Int,
               offsetY :: Int,
               nextFigIndex :: Int,
               nextFigColor :: Int,
               score :: Int
             }
instance Show State where
  show s = intercalate " " 
           [
             (show $ move s),
             (board s),
             (show $ figIndex s),
             (show $ rotateIndex s),
             (show $ color s),
             (show $ offsetX s),
             (show $ offsetY s),
             (show $ nextFigIndex s),
             (show $ nextFigColor s),
             (show $ score s)
           ]

parseBoard :: String -> Board
parseBoard b = b
parseState :: [String] -> State
parseState args = State
  (toEnum . read $ args !! 0)
  (parseBoard $ args !! 1)
  (read $ args !! 2)
  (read $ args !! 3)
  (read $ args !! 4)
  (read $ args !! 5)
  (read $ args !! 6)
  (read $ args !! 7)
  (read $ args !! 8)
  (read $ args !! 9)
  

update :: State -> State
update @state(State{
move=move,
board=board,
figIndex=figIndex,
rotateIndex=rotateIndex,
color=color,
offsetX=offsetX,
offsetY=offsetY,
nextFigIndex=nextFigIndex,
nextFigColor=nextFigColor,
score=score
}) = state

printState :: State -> String
printState s = show s

render :: State -> String
render s = printState s
