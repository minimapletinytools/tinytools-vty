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
import Data.These
import Data.Align (align)



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

onlyIfSimultaneous :: (Reflex t) => Event t a -> Event t b -> Event t a
onlyIfSimultaneous eva evb = fforMaybe (align eva evb) $ \case
  These a _ -> Just a
  _ -> Nothing


-- TODO write UTs
-- TODO reduce constraints
vScrollBar :: forall t m a. (MonadWidget t m)
  => Behavior t V.Attr -- ^ scroll bar style
  -> Dynamic t Int -- ^ content height
  -> m (Dynamic t Int) -- ^ offset
vScrollBar handleStyleBeh contentSizeDyn = mdo
  maxSizeDyn <- displayHeight
  let
    screen_over_content_dyn :: Dynamic t Float = liftA2 (\a b -> fromIntegral a / fromIntegral b ) maxSizeDyn contentSizeDyn
    maxSizeDiffDyn = liftA2 (-) maxSizeDyn boxHeightDyn

    maxContentSizeDiffDyn = fromIntegral . max 0 <$> liftA2 (-) contentSizeDyn maxSizeDyn

    boxHeightDyn = fmap ceiling $ liftA2 (*) screen_over_content_dyn (fromIntegral <$> maxSizeDyn)
    boxRegionDyn = Region <$> 0 <*> offsetScreenUnitDyn <*> 1 <*> boxHeightDyn

  --innerDragEv will only fire on drag events that started on the scroll bar handle portion
  innerDragEv <- pane boxRegionDyn (constDyn True) $ do
    -- render the scroll bar handle
    fill (constant '#')
    drag2 V.BLeft

  d2ev <- drag2 V.BLeft
  let
    moveDragEv = fmapMaybe (\d2 -> if _drag2_state d2 == Dragging then Just d2 else Nothing) d2ev
  lastDrag <- holdDyn emptyDrag2 d2ev
  let
    deltaDragEv_d1' = attach (current lastDrag) moveDragEv
    deltaDragEv_d1 = fmap (\(pd,d) -> _drag2_to d `componentSub` _drag2_to pd) deltaDragEv_d1'
    -- only process the event if they are simultaneous with innerDragEv (thus meaning they started on the scroll bar handle)
    -- the reason we need to do it this way is because `pane` messes with the mouse coords so we need to get the mouse coords from outside
    deltaDragEv = onlyIfSimultaneous (fmap snd deltaDragEv_d1) innerDragEv

  let
    content_over_screen_dyn = fmap (\x -> 1 / x) screen_over_content_dyn
    dragDeltaAdjustedEv = fmap (\(x,y) -> x * fromIntegral y) (attach (current content_over_screen_dyn) deltaDragEv)

  -- TODO movement when you click on areas off the bar
  -- TODO maybe do ^ v arrows at top and bottom to click scroll through 1 at a time

  -- TODO ugg you probably need an inputCaptured event here :\ (or you could just get rid of keyboard movement...)
  -- keyboard/scroll movement
  kup <- key V.KUp
  kdown <- key V.KDown
  mscroll <- mouseScroll
  let
    requestedScroll :: Event t Float
    requestedScroll = leftmost
      [ 1 <$ kdown
      , (-1) <$ kup
      , ffor mscroll $ \case
          ScrollDirection_Up -> (-1)
          ScrollDirection_Down -> 1
      ]

  -- then put it all together
  let
    foldOffsetFn (maxdiff, delta) c = max 0 (min maxdiff (c+delta))
  offsetFloatDyn <- foldDyn foldOffsetFn 0 (attach (current maxContentSizeDiffDyn) (leftmost [dragDeltaAdjustedEv, requestedScroll]))

  let
    offsetScreenUnitDyn = fmap round . liftA2 (*) screen_over_content_dyn $ offsetFloatDyn

  return $ fmap floor offsetFloatDyn


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
