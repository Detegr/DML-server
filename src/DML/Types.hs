{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module DML.Types(Resource(..),
                 Card(..),
                 Market(Market), slaves, spices, iron, wood,
                 BlackMarket,
                 DragonsLoot,
                 EventDeck,
                 Treasury,
                 House,
                 EventType,
                 Event,
                 mkEvent,
                 mkDeck,
                 mkPlayers,
                 Player(Player),
                 DMLState(DMLState), deck, market, bMarkets, loot, players, event,
                 mkCard,
                 ) where

import Data.Default
import Control.Lens.TH
import Data.Maybe (fromJust)
import Data.Random.Extras (shuffle)
import Data.Random.RVar
import Data.Random.Source
import Data.Random.Source.DevRandom

-- | Type representing a resource in a DML game
data Resource = Slave | Spice | Iron | Wood | Joker deriving (Show,Eq)

-- | Type representing a character card in a DML game
data Character = MotherOfDragons | Archudess | DragonEmpress | BitchQueen
               | TroublesomeBlabbermouth | GoodsSwindler | GrandInquisitor | BoonLiquidator deriving (Show, Eq)

-- | Type representing a card in a DML game
data Card = Card      { resource  :: Resource, value :: Int }
          | Guild     { resource  :: Resource               }
          | Character { character :: Character              }
          | DragonEgg { resource  :: Resource               }
          | Thief
          | King      { resource  :: Resource               } deriving (Show,Eq)

-- | Type representing a global event type in a DML game
data EventType = Decadence | TaxRelief | Looters | SupplyShortage | ForeignMerchant deriving (Show,Eq)

-- | Type representing a global event in a DML game
data Event = Event { etype :: EventType , power :: Int} deriving (Show,Eq)

-- | Type representing the market in a DML game.
data Market = Market { _slaves :: [Card]
                     , _spices :: [Card]
                     , _iron   :: [Card]
                     , _wood   :: [Card]
                     } deriving (Show,Eq)
makeLenses ''Market

instance Default Market where
  def = Market [] [] [] []

type BlackMarket = [Card]
type DragonsLoot = [Card]
type EventDeck = [Card]
type Treasury = [Card]
type House = [Card]

data Player = Player { treasury :: Treasury
                     , house :: House
                     , king :: Card
                     } deriving (Show,Eq)

data DMLState = DMLState { _deck     :: EventDeck
                         , _market   :: Market
                         , _bMarkets :: [BlackMarket]
                         , _loot     :: DragonsLoot
                         , _players  :: (Player, Player, Player, Player)
                         , _event    :: Maybe Event
                         } deriving (Show)
makeLenses ''DMLState

-- | Constructs jack 'Character' from a 'Resource'
getJack :: Resource -> Character
getJack Slave = TroublesomeBlabbermouth
getJack Spice = GoodsSwindler
getJack Iron = GrandInquisitor
getJack Wood = BoonLiquidator

-- |Constructs queen 'Character' from a 'Resource'
getQueen :: Resource -> Character
getQueen Slave = MotherOfDragons
getQueen Spice = Archudess
getQueen Iron = DragonEmpress
getQueen Wood = BitchQueen

-- |Constructs 'EventType' from a 'Resource'
getEvent :: Resource -> EventType
getEvent Slave = Decadence
getEvent Spice = TaxRelief
getEvent Iron = Looters
getEvent Wood = SupplyShortage
getEvent Joker = ForeignMerchant

-- |Constructs an 'Event' out of a 'Maybe Card'
mkEvent :: Maybe Card -> Maybe Event
mkEvent (Just (Card r v)) = Just $ Event (getEvent r) v
mkEvent _                 = Nothing

-- | Constructs a 'Card' out of 'Resource' and a value
mkCard :: Resource -> Int -> Maybe Card
mkCard r v
  | r == Joker      = Just Thief
  | v > 1 && v < 10 = Just $ Card r v
  | v == 1          = Just $ DragonEgg r
  | v == 10         = Just $ Guild r
  | v == 11         = Just $ Character (getJack r)
  | v == 12         = Just $ Character (getQueen r)
  | v == 13         = Just $ King r
  | otherwise       = Nothing

-- | Creates a deck of cards and shuffles it
mkDeck :: RandomSource m DevRandom => m [Card]
mkDeck = do
  let orderedDeck = Thief : Thief : [fromJust $ mkCard r v | r <- [Slave,Spice,Iron,Wood], v <- [1..12]]
  runRVar (shuffle orderedDeck) DevURandom

mkPlayer :: Card -> Player
mkPlayer (King r) = Player [] [] (King r)

-- | Creates a tuple of players
mkPlayers :: RandomSource m DevRandom => m (Player, Player, Player, Player)
mkPlayers = do
  let kingDeck = [fromJust $ mkCard r 13 | r <- [Slave, Spice, Iron, Wood]]
  kings <- runRVar (shuffle kingDeck) DevURandom
  return (mkPlayer $ kings !! 0, mkPlayer $ kings !! 1, mkPlayer $ kings !! 2, mkPlayer $ kings !! 3) -- ugly :(
