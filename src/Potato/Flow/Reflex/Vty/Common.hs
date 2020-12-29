{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Common (
  radioList
  , radioListSimple
) where

import           Relude
import qualified Relude.Unsafe                      as Unsafe

import           Potato.Flow.Controller
import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Control.Monad.NodeId
import qualified Data.List.Index                    as L
import qualified Data.Text                          as T

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty

radioList :: forall t m. (Reflex t, MonadNodeId m)
  => Dynamic t [Text] -- ^ list of button contents
  -> Dynamic t [Int] -- ^ which buttons are "active"
  -> VtyWidget t m (Event t Int) -- ^ event when button is clickd
radioList buttonsDyn activeDyn = do
  dw <- displayWidth
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
      attr = lg_default --if selected then lg_layer_selected else lg_default
      c = if selected then "{" <> t <> "}" else "[" <> t <> "]"
  tellImages $ fmap (fmap makeImage) $ current buttons
  return $ flip push mouseDownEv $ \(MouseDown _ (px,py) _) -> do
    bs <- sample . current $ buttons
    return $ L.ifindIndex (\_ ((x,y,l),_,_) -> py == y && px >= x && px < x+l) bs


radioListSimple :: forall t m. (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m)
  => Int -- ^ initial choice
  -> [Text] -- ^ list of button contents (must be at least one)
  -> VtyWidget t m (Dynamic t Int) -- ^ which radio is selected
radioListSimple initial buttons = mdo
  radioEvs <- radioList (constDyn buttons) radioDyn
  radioDyn <- holdDyn [0] $ fmap (\x->[x]) radioEvs
  return $ fmap (Unsafe.head) radioDyn
