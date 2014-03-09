import Control.Lens hiding (elements)
import Data.Maybe
import Test.QuickCheck
import Text.Printf

import DML.Types
import DML.Game

resourceGen :: Gen Resource
resourceGen = elements [Slave, Spice, Iron, Wood, Joker]

instance Arbitrary Card where
  arbitrary = do
    resource <- resourceGen
    value <- choose (1,12) -- Do not generate kings
    case mkCard resource value of
      Nothing -> arbitrary
      Just card -> return card

instance Arbitrary Market where
  arbitrary = do
    l1 <- choose(0,54)
    l2 <- choose(0,54-l1)
    l3 <- choose(0,54-l1+l2)
    l4 <- choose(0,54-l1+l2+l3)
    m1 <- vector l1
    m2 <- vector l2
    m3 <- vector l3
    m4 <- vector l4
    return $ Market m1 m2 m3 m4

instance Arbitrary DMLState where
  arbitrary = do
    len <- choose (0,54)
    len2 <- choose (0,54-len)
    len3 <- choose (0,54-len+len2)
    deck <- vector len
    market <- (arbitrary :: Gen Market)
    bMarket <- vector len2
    loot <- vector len3
    pv <- vector 4
    let players = (pv !! 0, pv !! 1, pv !! 2, pv !! 3)
    return $ DMLState deck market bMarket loot players Nothing

instance Arbitrary Player where
  arbitrary = return $ Player [] [] (fromJust $ mkCard Slave 13)

prop_draw :: DMLState -> Bool
prop_draw s = length drawnDeck == (length fullDeck) - 1 || (length fullDeck) == 0
  where (_, s') = draw s
        fullDeck = s ^. deck
        drawnDeck = s' ^. deck

main :: IO()
main = quickCheck prop_draw
