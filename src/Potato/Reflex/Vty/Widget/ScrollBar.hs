{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Widget.ScrollBar (
  vScrollBar
) where

import           Relude

import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import qualified Graphics.Vty as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

import           Data.Default
import qualified Data.Sequence as Seq
import Data.Fixed (div')



emptyDrag2 :: Drag2
emptyDrag2 = Drag2  {
    _drag2_from       = (0,0)
    , _drag2_to        = (0,0)
    , _drag2_button    = V.BLeft
    , _drag2_modifiers = []
    , _drag2_state     = DragStart
  }

componentSub :: (Num a) => (a,a) -> (a,a) -> (a,a)
componentSub (a,b) (c,d) = (a-c,b-d)


-- TODO write UTs
-- TODO reduce constraints
vScrollBar :: forall t m a. (MonadWidget t m)
  => Behavior t V.Attr -- ^ scroll bar style
  -> Dynamic t Int -- ^ content height
  -> m (Dynamic t Int) -- ^ offset
vScrollBar handleStyleBeh contentSizeDyn = mdo
  maxSizeDyn <- displayHeight
  let
    ratioDyn :: Dynamic t Float = liftA2 (\a b -> fromIntegral a / fromIntegral b ) maxSizeDyn contentSizeDyn
    maxSizeDiffDyn = liftA2 (-) maxSizeDyn boxHeightDyn

    boxHeightDyn = fmap ceiling $ liftA2 (*) ratioDyn (fromIntegral <$> maxSizeDyn)
    boxRegionDyn = Region <$> 0 <*> traceDyn "poop" offsetDyn <*> 1 <*> boxHeightDyn

  deltaDragEv <- pane boxRegionDyn (constDyn True) $ do
    fill (constant '#')
    -- this works and I don't know why because the pane is moving when you move the mouse and the mouse is relative to the pane 🤷🏼‍♀️
    d2ev <- drag2 V.BLeft
    let
      moveDragEv = fmapMaybe (\d2 -> if _drag2_state d2 == Dragging then Just d2 else Nothing) d2ev
    lastDrag <- holdDyn emptyDrag2 d2ev
    let
      deltaDragEv_d1' = attach (current lastDrag) moveDragEv
      deltaDragEv_d1 = fmap (\(pd,d) -> _drag2_to d `componentSub` _drag2_to pd) deltaDragEv_d1'
    return $ fmap snd deltaDragEv_d1

  -- TODO movement when you click on areas off the bar

  -- keyboard/scroll movement
  kup <- key V.KUp
  kdown <- key V.KDown
  --inp <- input
  mscroll <- mouseScroll
  let
    requestedScroll :: Event t Int
    --requestedScroll = traceEvent "boop" $ leftmost
    requestedScroll = leftmost
      [ 1 <$ kdown
      , (-1) <$ kup
      , ffor mscroll $ \case
          ScrollDirection_Up -> (-1)
          ScrollDirection_Down -> 1
      ]

  -- then put it all together
  let
    --foldOffsetFn (maxdiff, delta) c = trace (show c <> " " <> show delta <> " " <> show maxdiff) $ max 0 (min maxdiff (c+delta))
    foldOffsetFn (maxdiff, delta) c = max 0 (min maxdiff (c+delta))
  offsetDyn <- foldDyn foldOffsetFn 0 (attach (current maxSizeDiffDyn) (leftmost [deltaDragEv, requestedScroll]))

  return offsetDyn


{-

  data DragState = DragStart | Dragging | DragEnd deriving (Eq, Ord, Show)

  -- | Same as 'Drag' but able to track drag start case
  data Drag2 = Drag2
    { _drag2_from      :: (Int, Int) -- ^ Where the drag began
    , _drag2_to        :: (Int, Int) -- ^ Where the mouse currently is
    , _drag2_button    :: V.Button -- ^ Which mouse button is dragging
    , _drag2_modifiers :: [V.Modifier] -- ^ What modifiers are held
    , _drag2_state     :: DragState -- ^ Whether the drag ended (the mouse button was released)
    }
    deriving (Eq, Ord, Show)

  -- | Same as 'drag' but returns 'Drag2' which tracks drag start events
  drag2
    :: (Reflex t, MonadFix m, MonadHold t m, HasInput t m)
    => V.Button
    -> m (Event t Drag2)
  drag2 btn = mdo-}
