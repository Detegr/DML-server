module Main where

import Control.Lens
import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import DML.Game
import DML.Types
import Data.Default

main :: IO()
main = do
  gen <- fmap mkStdGen randomIO
  initialDeck <- mkDeck
  playerTuple <- mkPlayers

  let initialState = DMLState initialDeck def [] [] playerTuple Nothing

  let (_,st)=runDML initialState gen $ do
               restockM
               supplyM
               supplyM
               supplyM
               supplyM
               supplyM
  putStrLn . show $ (st ^. market)
