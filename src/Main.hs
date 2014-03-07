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
  players <- mkPlayers
  print $ deck

  let initialState = DMLState { deck     = deck
                              , market   = def
                              , bMarkets = []
                              , loot     = []
                              , players  = players
                              , event    = Nothing
                              }
  let roll=evalDML mkRoll initialState gen
  putStrLn $ show roll
