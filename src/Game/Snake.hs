{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Game.Snake where

import Control.Applicative ((<|>))
import Control.Lens
  ( makeLenses,
    modifying,
    use,
    (%~),
    (&),
    (.=),
    (.~),
    (^.),
  )
import Control.Monad (guard)
import Control.Monad.Extra (orM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.State (State, execState, get, modify)
import Data.Maybe (fromMaybe)
import Data.Sequence
  ( Seq (..),
    ViewL (..),
    ViewR (..),
    (<|),
  )
import qualified Data.Sequence as S
import Linear.V2
  ( V2 (..),
    _x,
    _y,
  )
import System.Random
  ( Random (..),
    newStdGen,
  )

------------------------------------------------------------------------
-- Types

data Game = Game
  { -- | snake as a sequence of points in R2
    _snake :: Snake,
    -- | direction
    _dir :: Direction,
    -- | location of the food
    _food :: Coord,
    -- | infinite list of random food locations
    _foods :: Stream Coord,
    -- | game over flag
    _dead :: Bool,
    -- | paused flag
    _paused :: Bool,
    -- | score
    _score :: Int,
    -- | freeze to disallow duplicate turns
    _frozen :: Bool
  }
  deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

------------------------------------------------------------------------
-- Constants

width, height :: Int
width = 20
height = 20

------------------------------------------------------------------------
-- Functions

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
