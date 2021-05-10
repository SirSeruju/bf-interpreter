module BrainFuck where

import Data.Functor
import Control.Applicative
import Data.Char

data Op
  = Add       -- +
  | Sub       -- -
  | Next      -- >
  | Prev      -- <
  | Out       -- .
  | In        -- ,
  | Loop [Op] -- [...]

type Cells = ([Int], [Int])

initCells :: Cells
initCells = (repeat 0, repeat 0)

runOp :: Op -> Cells -> IO Cells
runOp Add (a, x : xs) = return (a, (x + 1) : xs)
runOp Sub (a, x : xs) = return (a, (x - 1) : xs)
runOp Next (a, x : xs) = return (x : a, xs)
runOp Prev (x : xs, a) = return (xs, x : a)
runOp Out c@(a, x : xs) = do
  putChar $ chr x
  return c
runOp In (a, x : xs) = do
  newX <- getChar
  return (a, (ord newX) : xs)
runOp loop@(Loop ops) cells@(a, x : xs) = do
  if x == 0 then
    return cells
  else do
    newCells <- f ops cells
    runOp loop newCells

  where
    f [] cells = return cells
    f (x : xs) cells = do
      newCells <- runOp x cells
      f xs newCells
      

runOps :: [Op] -> IO ()
runOps ops = f ops initCells
  where
    f [] cells = return ()
    f (x : xs) cells = do
      newCells <- runOp x cells
      f xs newCells

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Next = ">"
  show Prev = "<"
  show Out = "."
  show In = ","
  show (Loop ops) = "[" ++ (concat $ map show ops) ++ "]"
