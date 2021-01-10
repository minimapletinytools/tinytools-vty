{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Widget.Windows (

) where

import           Relude

import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import qualified Graphics.Vty.Input.Events as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

import qualified Data.Map as Map (empty)
import           Data.Default
import Control.Monad.Fix

type WidgetId = Int

data WindowsAttrs t = WindowsAttrs {

}

data Window = Window {
  _window_name :: Text
  , _window_widgetId :: WidgetId
}

-- note, OneWindow can not have tabs added to it
data Tab = OneWindow Window | Tab [Window]

data DockDirection =
  DockDirection_Left
  | DockDirection_Right
  | DockDirection_Top
  | DockDirection_Bottom
  deriving (Show)

data DockedTab = DockedTab {
  _dockedTab_tabs :: [(Int, Tab)] -- left to right, or top to bottom
  , _dockedTab_size :: Int
  , _dockedTab_dir :: DockDirection
}

data FreeWindow = FreeWindow {
  _freeWindow_window :: Window
  , _freeWindow_position :: (Int, Int)
  , _freeWindow_size :: (Int, Int)
}

data WindowManagerState t m a = WindowManagerState {
  _windowManagerState_docked :: [DockedTab]
  , _windowManagerState_free :: [FreeWindow]
  , _windowManagerState_size :: Dimension
  , _windowManagerState_widgetMap :: Map WidgetId (VtyWidget t m a)
}

emptyWindowManagerState :: WindowManagerState t m a
emptyWindowManagerState = WindowManagerState {
    _windowManagerState_docked = []
    , _windowManagerState_free = []
    , _windowManagerState_size = (0,0)
    , _windowManagerState_widgetMap = Map.empty
  }

-- temp math stuff
type Position = (Int, Int)
type Dimension = (Int, Int)
type PosDim = (Position, Dimension)

--(:+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
--(a,b) :+ (x,y) = (a+x, b+y)
--infixl 6 :+
--(-+) :: (Int, Int) -> (Int, Int) -> (Int, Int)
--(a,b) :+ (x,y) = (a-x, b-y)
--infixl 6 -+

computeDockDimensions :: PosDim -> [DockedTab] -> [PosDim]
computeDockDimensions dim = snd . mapAccumL mapAccumFn dim where
  mapAccumFn ((accx, accy), (accw, acch)) dt = (newAccDim, dtpd) where
    (dtpd, newAccDim) = case _dockedTab_dir dt of
      DockDirection_Left -> (
          ((accx, accy), (dw, acch))
          , ((accx+dw, accy), (accw-dw, acch))
        )
      DockDirection_Right -> (
          ((accx + accw - dw, accy), (dw, acch))
          , ((accx, accy), (accw-dw, acch))
        )
      DockDirection_Top -> (
          ((accx, accy), (accw, dh))
          , ((accx, accy+dh), (accw, acch-dh))
        )
      DockDirection_Bottom -> (
          ((accx, accy + acch - dh), (accw, dh))
          , ((accx, accy), (accw, acch-dh))
        )
      where
        dw = min accw (_dockedTab_size dt)
        dh = min acch (_dockedTab_size dt)



data WindowManagerConfig t m a = WindowManagerConfig {
 _windowManagerConfig_initialWidgets :: Map WidgetId (VtyWidget t m a)

 -- TODO initial widget configuration

 , _windowManagerConfig_style :: WindowsAttrs t

 -- eventually
 --, _windowManagerConfig_addWidget :: Event t
}

data WMCmd = WMCmd_None

windowManager ::
  forall t m a. (Reflex t, MonadFix m, MonadHold t m, Monad m)
  => WindowManagerConfig t m a
  ->  VtyWidget t m (Event t a)
windowManager WindowManagerConfig {..} = mdo

  inpEv <- input
  widthDyn <- displayWidth
  heightDyn <- displayHeight
  initialWidth <- sample . current $ widthDyn
  initialHeight <- sample . current $ heightDyn

  let
    cmdev = never
    foldfn :: WMCmd -> WindowManagerState t m a -> WindowManagerState t m a
    foldfn cmd wms@WindowManagerState {..} = r where
      r = wms

    initialState = emptyWindowManagerState {
        _windowManagerState_size = (initialWidth, initialHeight)
      }

  wmsDyn <- foldDyn foldfn initialState cmdev
  return never

-- TODO monad for making initial configuration
{-
dock = do
  free $ widget1
  free $ widget2
  free $ widget3
  dock DockDirection_Left $ do
    addTab $ do
      tab $ widget4
      tab $ widget5
    addTab $ do
      tab $ widget6
      tab $ widget7
  dock DockDirection_Bottom $ do
    addWindow $ widget8
-}
