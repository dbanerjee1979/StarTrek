module Main where

import GameController (execGame)
import Data.List

main :: IO ()
main = do
  (askPlayAgain, msg) <- execGame
  putStrLn $ intercalate "\n" msg
  if askPlayAgain
    then do putStrLn "The federation is in need of a new starship commander for a similar mission"
            putStrLn "If there is a volunteer, let him step forward and enter 'AYE': "
            resp <- getLine
            if resp == "AYE"
              then main
              else return ()
    else return ()

