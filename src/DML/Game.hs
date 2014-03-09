module DML.Game where

import Control.Applicative ((<$>),(<*>))
import Control.Lens
import Control.Monad.Random
import Control.Monad.Trans (lift)
import Control.Monad.Trans.State
import Debug.Trace
import DML.Types

type DML g a = RandT g (State DMLState) a

runDML :: (RandomGen g) => DMLState -> g -> DML g a -> (a, DMLState)
runDML s g f = runState (evalRandT f g) s

evalDML :: (RandomGen g) => DMLState -> g -> DML g a -> a
evalDML s g f = fst $ runDML s g f

execDML :: (RandomGen g) => DMLState -> g -> DML g a -> DMLState
execDML s g f = snd $ runDML s g f

-- |Lifts a function modifying DMLState to DML monad
liftDML :: (DMLState -> DMLState) -> DML g ()
liftDML f = lift get >>= lift . put . f >> return ()

-- |Lifts a function modifying DMLState to DML monad
liftDML' :: (DMLState -> (a, DMLState)) -> DML g a
liftDML' f = lift get >>= \s -> let (a,s') = f s in lift $ put s' >> return a

-- |Draws a card from 'EventDeck'.
--
-- If 'EventDeck' is empty, returns Nothing
draw :: DMLState -> (Maybe Card, DMLState)
draw s = if null deck'
           then (Nothing, s)
           else (Just c, draw' s)
  where deck' = s ^. deck
        c = head deck'
        rest = tail deck'
        draw' = deck .~ rest

-- |Executes 'draw' in a DML monad
drawM :: DML g (Maybe Card)
drawM = liftDML' draw

-- |Draws a card from 'EventStack' and assigns it as the
-- current global event if the card is a valid event card
drawEvent :: DMLState -> DMLState
drawEvent s = let (c,s') = draw s in event .~ (mkEvent c) $ s'

-- |Executes 'drawEvent' in a DML monad
drawEventM :: DML g ()
drawEventM = liftDML drawEvent

-- |Supply phase of a DML game.
--
-- A card is drawn from 'EventDeck' and added either to 'Market' or 'DragonsLoot'
supply :: DMLState -> DMLState
supply s =
  case draw s of
    (Nothing, st) -> st
    (Just c, st) ->
      case c of
        DragonEgg _ -> addToLoot c st
        Character _ -> addToLoot c st
        Guild     _ -> addToLoot c st
        Card    _ _ -> addToMarket c st
        Thief       -> addToLoot c st
        --King      _ -> undefined
  where addToLoot c = loot <>~ [c]
        addToMarket c =
          case (resource c) of
            Slave -> over market $ slaves <>~ [c]
            Spice -> over market $ spices <>~ [c]
            Iron  -> over market $ iron <>~ [c]
            Wood  -> over market $ wood <>~ [c]
            --Joker -> undefined

-- |Executes 'supply' in a DML monad
supplyM :: DML g ()
supplyM = liftDML supply

marketSize :: Market -> Int
marketSize m = m1 + m2 + m3 + m4
  where m1 = length $ m ^. slaves
        m2 = length $ m ^. spices
        m3 = length $ m ^. iron
        m4 = length $ m ^. wood

-- |Restock phase of a DML game
--
-- The market is restocked. Cards are drawn to the market using 'supply'
-- until there's 3 cards in the market.
restock :: DMLState -> DMLState
restock s = if count < 3
              then restock (supply s)
              else s
  where count = marketSize (s ^. market)

-- |Executes 'restock' in a DML monad
restockM :: DML g ()
restockM = liftDML restock

-- |Rolls two dice and returns the values in a tuple
rollM :: (RandomGen g) => DML g (Int, Int)
rollM = (,) <$> rollOne <*> rollOne
  where range = (1,6)
        rollOne = getRandomR range
