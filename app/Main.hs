module Main where

import Parser
import BrainFuck
import Data.Functor
import Control.Applicative
import Data.Char
import Data.Maybe

opP :: Parser Op
opP = addP <|> subP <|> nextP <|> prevP <|> outP <|> inP <|> loopP
  where
    addP  = charP '+' $> Add
    subP  = charP '-' $> Sub
    nextP = charP '>' $> Next
    prevP = charP '<' $> Prev
    outP  = charP '.' $> Out
    inP   = charP ',' $> In
    loopP = Loop <$> (charP '[' *> opsP <* charP ']')

opsP :: Parser [Op]
opsP = many opP

main :: IO ()
main = do
  putStrLn "Brainfuck code:"
  code <- getLine
  if isNothing (runParser opsP code) then
    putStrLn "Wrong code."
  else
    runOps $ fromMaybe undefined (snd <$> runParser opsP code)
  return ()
