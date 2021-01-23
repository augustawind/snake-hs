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
    _locked :: Bool
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
-- step g = fromMaybe g $ do
--   guard (not $ g ^. paused || g ^. dead)
--   let g' = g & locked .~ False
--   return . fromMaybe (move g') $ die g' <|> eatFood g'
step g = flip execState g . runMaybeT $ do
  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]
  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False
  -- Die, eat, or move
  die <|> eatFood <|> MaybeT (Just <$> modify move)

-- | Die if next head position is illegal.
die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> use snake
  MaybeT . fmap Just $ dead .= True

-- | Eat food if next head position is food.
eatFood :: MaybeT (State Game) ()
eatFood = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> use food
  MaybeT . fmap Just $ do
    modifying score (+ 10)
    get >>= \g -> modifying snake (nextHead g <|)
    nextFood

-- | Set a valid next food coordinate.
nextFood :: State Game ()
nextFood = do
  (f :| fs) <- use foods
  foods .= fs
  use snake
    >>= ( \case
            True -> nextFood
            False -> food .= f
        )
      . elem f

-- | Move snake along in a marquee fashion.
move :: Game -> Game
move g@Game {_snake = (s :|> _)} = g & snake .~ (nextHead g <| s)
move _ = error "Snakes can't be empty!"

-- | Get next head position of the snake.
nextHead :: Game -> Coord
nextHead Game {_dir = d, _snake = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height)
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West = a & _x %~ (\x -> (x - 1) `mod` width)
nextHead _ = error "Snakes can't be empty!"

-- | Turn game direction (only turns orthogonally).
--
-- Implicitly unpauses yet freezes game.
turn :: Direction -> Game -> Game
turn d g =
  if g ^. locked
    then g
    else g & dir %~ turnDir d & paused .~ False & locked .~ True
  where
    turnDir :: Direction -> Direction -> Direction
    turnDir n c
      | c `elem` [North, South] && n `elem` [East, West] = n
      | c `elem` [East, West] && n `elem` [North, South] = n
      | otherwise = c

-- | Initialize a paused game with randomized food locations.
initGame :: IO Game
initGame = do
  (f :| fs) <- fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
      g =
        Game
          { _snake = S.singleton (V2 xm ym),
            _food = f,
            _foods = fs,
            _score = 0,
            _dir = North,
            _dead = False,
            _paused = True,
            _locked = False
          }
  return $ execState nextFood g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")
