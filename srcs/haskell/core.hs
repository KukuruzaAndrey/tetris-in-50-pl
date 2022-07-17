import System.Environment   
main :: IO () 
main = do
  args <- getArgs
  let state = parseState args
      nextState = update state
  putStrLn $ printState nextState
  putStrLn $ render nextState

data Move = Down | Left | Right | Rotate_C | Rotate_C_C | Drop
type Board = [[Int]]
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

parseBoard :: String -> [[Int]]
parseBoard = 
parseState :: [String] -> State
parseState args = State
  (toEnum . read $ args !! 0)
  (parseBoard $ args !! 1)
  

update :: State -> State
update s = State Rotate_C

printState :: State -> String
printState s = "state"

render :: State -> String
render s = "tetris"
