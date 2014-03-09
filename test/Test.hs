import Control.Lens hiding (elements)
import Data.Maybe
import Test.QuickCheck
import Text.Printf

import DML.Types
import DML.Game

resourceGen :: Gen Resource
resourceGen = elements [Slave, Spice, Iron, Wood, Joker]

isKing :: Card -> Bool
isKing (King _) = True
isKing _        = False

charOrGuild :: Card -> Bool
charOrGuild (Character _) = True
charOrGuild (Guild _)     = True
charOrGuild _             = False

cardGenNoKing :: Gen Card
cardGenNoKing = arbitrary `suchThat` (not . isKing)

cardGenKing :: Gen Card
cardGenKing = arbitrary `suchThat` isKing

cardGenCharOrGuild :: Gen Card
cardGenCharOrGuild = arbitrary `suchThat` charOrGuild

instance Arbitrary Card where
  arbitrary = do
    resource <- resourceGen
    value <- choose (1,13)
    case mkCard resource value of
      Nothing -> arbitrary
      Just card -> return card

instance Arbitrary Market where
  arbitrary = do
    l1 <- choose(0,54)
    l2 <- choose(0,54-l1)
    l3 <- choose(0,54-l1+l2)
    l4 <- choose(0,54-l1+l2+l3)
    m1 <- vectorOf l1 cardGenNoKing
    m2 <- vectorOf l2 cardGenNoKing
    m3 <- vectorOf l3 cardGenNoKing
    m4 <- vectorOf l4 cardGenNoKing
    return $ Market m1 m2 m3 m4

instance Arbitrary DMLState where
  arbitrary = do
    deck <- listOf cardGenNoKing
    market <- arbitrary
    bm1 <- listOf cardGenNoKing
    bm2 <- listOf cardGenNoKing
    bm3 <- listOf cardGenNoKing
    loot <- listOf cardGenNoKing
    pv <- vector 4
    let players = (pv !! 0, pv !! 1, pv !! 2, pv !! 3)
    return $ DMLState deck market [bm1,bm2,bm3] loot players Nothing

instance Arbitrary Player where
  arbitrary = do
    king <- cardGenKing
    treasury <- listOf cardGenNoKing
    house <- listOf cardGenCharOrGuild
    return $ Player [] [] king

prop_draw :: DMLState -> Bool
prop_draw s = length drawnDeck == (length fullDeck) - 1 || (length fullDeck) == 0
  where (_, s') = draw s
        fullDeck = s ^. deck
        drawnDeck = s' ^. deck

prop_supply :: DMLState -> Bool
prop_supply s = case card of
  Just c ->
    case c of
      DragonEgg _ -> checkLoot
      Character _ -> checkLoot
      Guild     _ -> checkLoot
      Card    r _ -> checkMarket r
      Thief       -> checkLoot
  Nothing ->
    length slaveMarket == length slaveMarket' &&
    length spiceMarket == length spiceMarket' &&
    length ironMarket  == length ironMarket'  &&
    length woodMarket  == length woodMarket'  &&
    length dragonsLoot == length dragonsLoot'
  where s' = supply s
        (card,_) = draw s
        slaveMarket = s ^. market . slaves
        slaveMarket' = s' ^. market . slaves
        spiceMarket = s ^. market . spices
        spiceMarket' = s' ^. market . spices
        ironMarket = s ^. market . iron
        ironMarket' = s' ^. market . iron
        woodMarket = s ^. market . wood
        woodMarket' = s' ^. market . wood
        dragonsLoot = s ^. loot
        dragonsLoot' = s' ^. loot
        checkPlusOne a b = length a == length b + 1
        checkLoot = checkPlusOne dragonsLoot' dragonsLoot
        checkMarket r =
          case r of
            Slave -> checkPlusOne slaveMarket' slaveMarket
            Spice -> checkPlusOne spiceMarket' spiceMarket
            Iron  -> checkPlusOne ironMarket' ironMarket
            Wood  -> checkPlusOne woodMarket' woodMarket

main :: IO()
main = mapM_ quickCheck [prop_draw, prop_supply]
