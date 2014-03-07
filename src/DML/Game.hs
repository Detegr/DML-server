module DML.Game where

import DML.Types
import Control.Monad.Random
import Control.Monad.Trans.State
import Control.Applicative ((<$>),(<*>))

type DML g a = RandT g (State DMLState) a

runDML :: (RandomGen g) => DML g a -> DMLState -> g -> (a, DMLState)
runDML f s g = runState (evalRandT f g) s

evalDML :: (RandomGen g) => DML g a -> DMLState -> g -> a
evalDML f s g = fst $ runDML f s g

execDML :: (RandomGen g) => DML g a -> DMLState -> g -> DMLState
execDML f s g = snd $ runDML f s g

supply :: DMLState -> DMLState
supply s
  | deck s == [] = s
  | otherwise = undefined
  where c = head (deck s)

-- | Rolls two dice and returns the values in a tuple
mkRoll :: (RandomGen g) => DML g (Int, Int)
mkRoll = (,) <$> rollOne <*> rollOne
  where range = (1,6)
        rollOne = getRandomR range
