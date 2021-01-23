{-# LANGUAGE OverloadedStrings #-}

module Game.Snake.UI where

import Brick
  ( App (..),
    AttrMap,
    AttrName,
    BrickEvent (..),
    EventM,
    Next,
    Padding (..),
    Widget,
    attrMap,
    continue,
    customMain,
    emptyWidget,
    fg,
    hBox,
    hLimit,
    halt,
    neverShowCursor,
    on,
    padAll,
    padLeft,
    padRight,
    padTop,
    str,
    vBox,
    vLimit,
    withAttr,
    withBorderStyle,
    (<+>),
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Concurrent (forkIO, threadDelay)
import Control.Lens ((^.))
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Game.Snake
import qualified Graphics.Vty as V
import Linear.V2 (V2 (..))

------------------------------------------------------------------------
-- Types

-- | A Tick marks the passing of time.
--
-- This is our custom event that will be constantly fed into the app.
data Tick
  = Tick

-- | Named resources.
--
-- Not currently used, but will be easier to refactor calling this "Name" now.
type Name = ()

data Cell
  = Snake
  | Food
  | Empty

------------------------------------------------------------------------
-- App definition

app :: App Game Tick Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = return,
      appAttrMap = const attrs
    }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      writeBChan chan Tick
      threadDelay delay
  g <- initGame
  vty <- mkVty
  void $ customMain vty mkVty (Just chan) app g
  where
    mkVty = V.mkVty V.defaultConfig

-- | Determines how fast the game moves.
delay :: Int
delay = 112500

------------------------------------------------------------------------
-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick) = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) =
  liftIO initGame >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc [])) = halt g
handleEvent g _ = continue g

------------------------------------------------------------------------
-- Rendering

drawUI :: Game -> [Widget Name]
drawUI g = [C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 11 $
    vBox
      [ drawScore (g ^. score),
        padTop (Pad 2) $ drawMessage g
      ]

drawScore :: Int -> Widget Name
drawScore n =
  withBorderStyle BS.unicodeBold
    . B.borderWithLabel (str "Score")
    . C.hCenter
    . padAll 1
    $ str (show n)

drawMessage :: Game -> Widget Name
drawMessage g
  | g ^. dead = msg "GAME OVER"
  | g ^. paused = msg "Use ←↓↑→\n or hjkl\n to play."
  | otherwise = emptyWidget
  where
    msg = withAttr attrMessage . C.hCenter . str

attrMessage :: AttrName
attrMessage = "attrMessage"

drawGrid :: Game -> Widget Name
drawGrid g =
  withBorderStyle BS.unicodeBold
    . B.borderWithLabel (str "Snake")
    $ vBox rows
  where
    rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0 .. width - 1]]
    drawCoord = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake = Snake
      | c == g ^. food = Food
      | otherwise = Empty

drawCell :: Cell -> Widget Name
drawCell Snake = withAttr attrSnake cw
drawCell Food = withAttr attrFood cw
drawCell Empty = withAttr attrEmpty cw

cw :: Widget Name
cw = str "  "

attrSnake, attrFood, attrEmpty :: AttrName
attrSnake = "attrSnake"
attrFood = "attrFood"
attrEmpty = "attrEmpty"

attrs :: AttrMap
attrs =
  attrMap
    V.defAttr
    [ (attrSnake, V.blue `on` V.blue),
      (attrFood, V.red `on` V.red),
      (attrMessage, fg V.red `V.withStyle` V.bold)
    ]
