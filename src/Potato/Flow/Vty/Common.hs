-- TODO move to widget folder
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Common (
  ffilterButtonIndex
  , buttonList
  , radioList
  , radioListSimple
  , checkBox
) where

import           Relude
import qualified Relude.Unsafe               as Unsafe

import           Potato.Flow.Controller
import           Potato.Flow.Vty.Attrs
import           Potato.Reflex.Vty.Helpers
import Potato.Reflex.Vty.Widget

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.List.Index             as L
import qualified Data.Text                   as T
import Data.Tuple.Extra

import qualified Graphics.Vty                as V
import           Reflex
import           Reflex.Vty


ffilterButtonIndex :: (Reflex t) => Int -> Event t Int -> Event t ()
ffilterButtonIndex i = fmapMaybe (\i' -> if i == i' then Just () else Nothing)


maximumlist :: [Int] -> Int
maximumlist = foldr (\x y ->if x >= y then x else y) (-1)

simpleDrag :: (Reflex t, MonadHold t m, MonadFix m, HasInput t m) => V.Button -> m (Event t ((Int, Int), (Int, Int)))
simpleDrag btn = do
  dragEv <- drag2 btn
  return $ flip push dragEv $ \d@(Drag2 (fromX, fromY) (toX, toY) _ mods ds) -> do
    return $ if ds == DragEnd
      then Just $ ((fromX, fromY), (toX, toY))
      else Nothing


-- | option to pass in height is a hack to work around circular dependency issues as when using Layout, displayWidth may be dependent on returned dynamic height
buttonList :: forall t m. (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasTheme t m)
  => Dynamic t [Text] -- ^ list of button contents
  -> Maybe (Dynamic t Int) -- ^ optional width (displayWidth is used if Nothing)
  -> m (Event t Int, Dynamic t Int) -- ^ (event when button is clicked, height)
buttonList buttonsDyn mWidthDyn = do
  dw <- case mWidthDyn of
    Nothing -> displayWidth
    Just widthDyn -> return widthDyn
  clickEv <- simpleDrag V.BLeft

  -- TODO the better version of this highlights button on mouse down and "clicks" so long as you don't drag off the button
  --dragPosDyn
  --isDraggingDyn

  let
    -- ((x,y,length), contents)
    buttons :: Dynamic t [((Int,Int,Int), Text, Bool)]
    buttons = ffor2 dw buttonsDyn $ fn where
      fn w bs = r where
        mapaccumfn (x,y) t = ((nextx, ny), ((nx,ny,buttonl),t, False)) where
          buttonl = T.length t + 2
          nextx' = x + buttonl
          (nx,ny,nextx) = if nextx' > w then (0,y+1, buttonl) else (x,y, nextx')
        (_,r) = mapAccumL mapaccumfn (0, 0) bs
    makeImage :: ((Int,Int,Int), Text, Bool) -> V.Image
    makeImage ((x,y,_), t, downclickTODO) = V.translate x y $ V.text' attr ("["<>t<>"]") where
      attr = if downclickTODO then lg_layer_selected else lg_default
    heightDyn = fmap ((+1) . maximumlist . fmap (snd3 . fst3)) buttons
    selectEv = flip push clickEv $ \((px,py),(ex,ey)) -> do
      bs <- sample . current $ buttons
      return $ L.ifindIndex (\_ ((x,y,l),_,_) -> py == y && ey == y && px >= x && ex >= x && px < x+l && ex < x+l) bs
  tellImages $ fmap (fmap makeImage) $ current buttons
  return $ (selectEv, heightDyn)

-- | option to pass in height is a hack to work around circular dependency issues as when using Layout, displayWidth may be dependent on returned dynamic height
-- override style: does not modify state internally, instead state must be passed back in
radioList :: forall t m. (Reflex t, MonadNodeId m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasTheme t m)
  => Dynamic t [Text] -- ^ list of button contents
  -> Dynamic t [Int] -- ^ which buttons are "active"
  -> Maybe (Dynamic t Int) -- ^ optional width (displayWidth is used if Nothing)
  -> m (Event t Int, Dynamic t Int) -- ^ (event when button is clicked, height)
radioList buttonsDyn activeDyn mWidthDyn = do
  dw <- case mWidthDyn of
    Nothing -> displayWidth
    Just widthDyn -> return widthDyn
  mouseDownEv <- mouseDown V.BLeft
  let
    -- ((x,y,length), contents)
    buttons' :: Dynamic t [((Int,Int,Int), Text)]
    buttons' = ffor2 dw buttonsDyn $ fn where
      fn w bs = r where
        mapaccumfn (x,y) t = ((nextx, ny), ((nx,ny,buttonl),t)) where
          buttonl = T.length t + 2
          nextx' = x + buttonl
          (nx,ny,nextx) = if nextx' > w then (0,y+1, buttonl) else (x,y, nextx')
        (_,r) = mapAccumL mapaccumfn (0, 0) bs
    buttons :: Dynamic t [((Int,Int,Int), Text, Bool)]
    buttons = ffor2 buttons' activeDyn $ fn where
      fn bs actives' = r where
        actives = reverse $ sort actives'
        ifoldrfn _ (l,t) (output, []) = ((l,t,False):output, [])
        ifoldrfn i (l,t) (output, a:as) = if i == a
          then ((l,t,True):output, as)
          else ((l,t,False):output, a:as)
        (r,_) = L.ifoldr ifoldrfn ([],actives) bs
    makeImage :: ((Int,Int,Int), Text, Bool) -> V.Image
    makeImage ((x,y,_), t, selected) = V.translate x y $ V.text' attr c where
      attr = if selected then lg_layer_selected else lg_default
      --c = if selected then "[" <> t <> "]" else " " <> t <> " "
      c = "["<>t<>"]"
    heightDyn = fmap ((+1) . maximumlist . fmap (snd3 . fst3)) buttons
    selectEv = flip push mouseDownEv $ \(MouseDown _ (px,py) _) -> do
      bs <- sample . current $ buttons
      return $ L.ifindIndex (\_ ((x,y,l),_,_) -> py == y && px >= x && px < x+l) bs
  tellImages $ fmap (fmap makeImage) $ current buttons
  return $ (selectEv, heightDyn)

radioListSimple :: forall t m. (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasTheme t m)
  => Int -- ^ initial choice
  -> [Text] -- ^ list of button contents (must be at least one)
  -> m (Dynamic t Int) -- ^ which radio is selected
radioListSimple initial buttons = mdo
  (radioEvs,_) <- radioList (constDyn buttons) radioDyn Nothing
  radioDyn <- holdDyn [0] $ fmap (\x->[x]) radioEvs
  return $ fmap (Unsafe.head) radioDyn



-- TODO focus + enter to select via keyboard
-- | creates a check box "[x]" in upper left corner of region
-- override style: does not modify state internally, instead state must be passed back in
checkBox
  :: forall t m. (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m, HasDisplayRegion t m, HasImageWriter t m, HasInput t m, HasTheme t m)
  => Dynamic t Bool
  -> m (Event t Bool)
checkBox stateDyn = do
  text (ffor (current stateDyn) (\s -> if s then "[x]" else "[ ]"))
  mouseDownEv <- mouseDown V.BLeft
  let toggleEv = fforMaybe mouseDownEv $ \(MouseDown _ (px,py) _) -> if px >= 0 && px < 3 && py == 0 then Just () else Nothing
  return $ tag (current (fmap not stateDyn)) toggleEv
