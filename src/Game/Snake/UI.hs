{-# LANGUAGE OverloadedStrings #-}

module Game.Snake.UI where

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (..), EventM, Next,
                                             Padding (..), Widget, attrMap,
                                             continue, customMain, emptyWidget,
                                             fg, hBox, hLimit, halt,
                                             neverShowCursor, on, padAll,
                                             padLeft, padRight, padTop, str,
                                             vBox, vLimit, withAttr,
                                             withBorderStyle, (<+>))
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Maybe                 (fromMaybe)
import           Data.Sequence              (Seq)
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((^.))
import           Linear.V2                  (V2 (..))

import           Game.Snake

------------------------------------------------------------------------
-- Types

-- | A Tick marks the passing of time.
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources.
--
-- Not currently used, but will be easier to refactor calling this "Name" now.
type Name = ()

data Cell = Snake | Food | Empty

------------------------------------------------------------------------
-- App definition

app :: App Game Tick Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = return
    , appAttrMap = const theMap
    }

main :: IO ()
main = undefined

------------------------------------------------------------------------
-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent = undefined

------------------------------------------------------------------------
-- Rendering

drawUI :: Game -> [Widget Name]
drawUI = undefined

theMap :: AttrMap
theMap = undefined
