module Main where

import Control.Monad.Random
import Control.Monad.Trans.State
import System.Random
import Data.Default
import DML.Types
import DML.Game

main :: IO()
main = do
  gen <- fmap mkStdGen randomIO
  deck <- mkDeck
  kings <- mkKings
  print $ deck

  let plr = Player [] [] (kings !! 0)
  let plrs = (plr,plr,plr,plr)
  let initialState=DMLState deck def [] [] plrs
  let roll=evalDML mkRoll initialState gen
  putStrLn $ show roll
