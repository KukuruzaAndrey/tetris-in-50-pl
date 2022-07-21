{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (Left, Right)
import System.Environment
import System.Random
import Data.List
import Data.Maybe

main :: IO ()
main = do
  args <- getArgs
  rg <- getStdGen
  let state = parseState args
      nextState = update state rg
  putStrLn $ printState nextState
  putStrLn $ render nextState

data Move = Down | Left | Right | Rotate_C | Rotate_C_C | Drop deriving (Enum)
instance Show Move where
  show = show . fromEnum

type Board = String

data State = INIT_STATE | State
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
parseState args = if length args == 1
                  then INIT_STATE
                  else State
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

getFigCoords :: Int -> Int -> Int -> Int -> Maybe [[Int]]
getFigCoords figIndex rotateIndex offsetX offsetY = Just [[0,0], [0,1], [1,1]]

boardCellsFree :: [[Int]] -> Board -> Bool
boardCellsFree coords board = False

canPlace :: State -> Bool
canPlace s = isJust coords && boardCellsFree (fromJust coords) (board s)
             where coords = getFigCoords (figIndex s) (rotateIndex s) (offsetX s) (offsetY s)


needNewFigure :: State -> Bool
needNewFigure s = not $ canPlace s

processNewFigure :: State -> State
processNewFigure s = s


update :: State -> StdGen -> State
update INIT_STATE rg = State
                       Down
                       (concat $ replicate 200 "0")
                       (fst $ randomR (0, 6) rg)
                       0
                       (fst $ randomR (1, 7) rg)
                       3
                       (-1)
                       (fst $ randomR (0, 6) rg)
                       (fst $ randomR (1, 6) rg)
                       0
update state@(State{..}) _ = case move of
  Down       -> if needNewFigure state
                then processNewFigure state
                else state {offsetY = succ offsetY}
  Left       -> state {offsetX = pred offsetX}
  Right      -> state {offsetX = succ offsetX}
  Rotate_C   -> state {rotateIndex = pred rotateIndex}
  Rotate_C_C -> state {rotateIndex = succ rotateIndex}
  Drop       -> state {offsetX = succ offsetX}

printState :: State -> String
printState s = show s

render :: State -> String
render s = printState s
