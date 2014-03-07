{-# LANGUAGE FlexibleContexts #-}

module DML.Types(Resource(..),
                 Card,
                 Market(..),
                 BlackMarket,
                 DragonsLoot,
                 EventDeck,
                 Treasury,
                 House,
                 EventType,
                 Event,
                 mkEvent,
                 mkDeck,
                 mkKings,
                 Player(..),
                 DMLState(..)) where

import System.Random (StdGen)
import Data.Default
import Data.Maybe (fromJust)
import Data.Random.RVar
import Data.Random.Extras (shuffle)
import Data.Random.Source
import Data.Random.Source.DevRandom

-- | Type representing a resource in a DML game
data Resource = Slave | Spice | Iron | Wood | Joker deriving (Show,Eq)

-- | Type representing a character card in a DML game
data Character = MotherOfDragons | Archudess | DragonEmpress | BitchQueen
               | TroublesomeBlabbermouth | GoodsSwindler | GrandInquisitor | BoonLiquidator deriving (Show, Eq)

-- | Type representing a card in a DML game
data Card = Card { resource :: Resource, value :: Int }
          | Guild { resource :: Resource }
          | Character { character :: Character }
          | DragonEgg { resource :: Resource }
          | Thief
          | King { resource :: Resource } deriving (Show,Eq)

-- | Type representing a global event type in a DML game
data EventType = Decadence | TaxRelief | Looters | SupplyShortage | ForeignMerchant deriving (Show,Eq)

-- | Type representing a global event in a DML game
data Event = Event { etype :: EventType , power :: Int} deriving (Show,Eq)

-- | Type representing the market in a DML game.
data Market = Market { slaves :: [Card]
                     , spices :: [Card]
                     , iron   :: [Card]
                     , wood   :: [Card]
                     } deriving (Show,Eq)
instance Default Market where
  def = Market [] [] [] []

type BlackMarket = [Card]
type DragonsLoot = [Card]
type EventDeck = [Card]
type Treasury = [Card]
type House = [Card]

data Player = Player { treasury :: Treasury, house :: House, king :: Card } deriving (Show,Eq)
data DMLState = DMLState { deck :: EventDeck
                         , market :: Market
                         , bMarket :: BlackMarket
                         , loot :: DragonsLoot
                         , players :: (Player, Player, Player, Player)
                         }

-- | Constructs jack Character from a Resource
getJack :: Resource -> Character
getJack Slave = TroublesomeBlabbermouth
getJack Spice = GoodsSwindler
getJack Iron = GrandInquisitor
getJack Wood = BoonLiquidator
getJack Joker = undefined

-- | Constructs queen Character from a Resource
getQueen :: Resource -> Character
getQueen Slave = MotherOfDragons
getQueen Spice = Archudess
getQueen Iron = DragonEmpress
getQueen Wood = BitchQueen
getQueen Joker = undefined

-- | Constructs EventType from a Resource
getEvent :: Resource -> EventType
getEvent Slave = Decadence
getEvent Spice = TaxRelief
getEvent Iron = Looters
getEvent Wood = SupplyShortage
getEvent Joker = ForeignMerchant

-- | Constructs an Event out of Card
mkEvent :: Card -> Maybe Event
mkEvent (Card r v) = Just $ Event (getEvent r) v
mkEvent _          = Nothing

-- | Constructs a Card out of Resource and a value
mkCard :: Resource -> Int -> Maybe Card
mkCard r v
  | v > 1 && v < 10 = Just $ Card r v
  | v == 1          = Just $ DragonEgg r
  | v == 10         = Just $ Guild r
  | v == 11         = Just $ Character (getJack r)
  | v == 12         = Just $ Character (getQueen r)
  | v == 13         = Just $ King r
  | r == Joker      = Just Thief
  | otherwise       = Nothing

-- | Creates a deck of cards and shuffles it
mkDeck :: RandomSource m DevRandom => m [Card]
mkDeck = do
  let deck = Thief : Thief : [fromJust $ mkCard r v | r <- [Slave,Spice,Iron,Wood], v <- [1..12]]
  runRVar (shuffle deck) DevURandom

-- | Creates a deck of kings and shuffles it
mkKings :: RandomSource m DevRandom => m [Card]
mkKings = do
  let deck = [fromJust $ mkCard r 13 | r <- [Slave, Spice, Iron, Wood]]
  runRVar (shuffle deck) DevURandom
