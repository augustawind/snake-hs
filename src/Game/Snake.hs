{-# LANGUAGE TemplateHaskell #-}

module Game.Snake where

import           Control.Applicative            ( (<|>) )
import           Control.Monad                  ( guard )
import           Data.Maybe                     ( fromMaybe )
import           Data.Sequence                  ( (<|)
                                                , Seq
                                                , ViewL(..)
                                                , ViewR(..)
                                                )
import qualified Data.Sequence                 as S
import           Lens.Micro                     ( (%~)
                                                , (&)
                                                , (.~)
                                                , (^.)
                                                )
import           Lens.Micro.TH                  ( makeLenses )
import           Linear.V2                      ( V2(..)
                                                , _x
                                                , _y
                                                )
import           System.Random                  ( Random(..)
                                                , newStdGen
                                                )

------------------------------------------------------------------------
-- Types

data Game = Game
  { _snake  :: Snake -- ^ snake as a sequence of points in R2
  , _dir    :: Direction -- ^ direction
  , _food   :: Coord -- ^ location of the food
  , _foods  :: Stream Coord -- ^ infinite list of random food locations
  , _dead   :: Bool -- ^ game over flag
  , _paused :: Bool -- ^ paused flag
  , _score  :: Int -- ^ score
  , _frozen :: Bool -- ^ freeze to disallow duplicate turns
  }
  deriving Show

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving Show

data Direction =
    North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

------------------------------------------------------------------------
-- Core functions

-- | Step forward in time.
step :: Game -> Game
step g = fromMaybe g $ do
  guard (not $ g ^. paused || g ^. dead)
  let g' = g & frozen .~ False
  return . fromMaybe (move g') $ die g' <|> eatFood g'

-- | Die if next head position is a wall.
die :: Game -> Maybe Game
die = undefined

-- | Eat food if next head position is food.
eatFood :: Game -> Maybe Game
eatFood = undefined

-- | Move snake along in a marquee fashion.
move :: Game -> Game
move = undefined

-- | Turn game direction (only turns orthogonally).
--
-- Implicitly unpauses yet freezes game.
turn :: Direction -> Game -> Game
turn = undefined

-- | Initialize a paused game with randomized food locations.
initGame :: IO Game
initGame = undefined
