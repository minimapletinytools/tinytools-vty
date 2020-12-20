{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Tools (
  Tool(..)
  , ToolWidgetConfig(..)
  , ToolWidget(..)
  , holdToolsWidget
) where

import           Relude

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


-- TODO prob broken
radioList :: forall t m. (Monad m, Reflex t, MonadNodeId m)
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



data ToolWidgetConfig t = ToolWidgetConfig {
  _toolWidgetConfig_pfctx  :: PFWidgetCtx t
  , _toolWidgetConfig_tool :: Dynamic t Tool
}

data ToolWidget t = ToolWidget {
  _toolWidget_setTool :: Event t Tool
}


onlyIfBeh :: (Reflex t) => Event t a -> Behavior t Bool -> Event t a
onlyIfBeh ev beh = fmapMaybe (\(b,e) -> if b then Just e else Nothing) $ attach beh ev


holdToolsWidget :: forall t m. (PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => ToolWidgetConfig t
  -> VtyWidget t m (ToolWidget t)
holdToolsWidget ToolWidgetConfig {..} = mdo

  radioEvs <- radioList (constDyn ["s","╬","□","/","T"]) (fmap ((:[]) . fromEnum) _toolWidgetConfig_tool)
  let
    selectB = void $ ffilter (==0) radioEvs
    panB = void $ ffilter (==1) radioEvs
    boxB = void $ ffilter (==2) radioEvs
    lineB = void $ ffilter (==3) radioEvs
    textB = void $ ffilter (==4) radioEvs

  let
    -- TODO DELETE, we don't do key press anymore
    {-
    allowKB = constant True
    keyPressEv' k = (flip fmapMaybe) (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \case
      V.EvKey (V.KChar k') [] | k' == k -> Just ()
      _ -> Nothing
    keyPressEv k = onlyIfBeh (keyPressEv' k) allowKB
    -}
    keyPressEv k = never

    setTool = leftmost
      [Tool_Select <$ leftmost [ selectB, keyPressEv 'v']
      , Tool_Pan <$ leftmost [panB, keyPressEv ' ']
      , Tool_Box <$ leftmost [boxB, keyPressEv 'b']
      , Tool_Line <$ leftmost [lineB, keyPressEv 'l']
      , Tool_Text <$ leftmost [textB, keyPressEv 't']]

  vLayoutPad 4 $ debugStream [
    never
    , fmapLabelShow "radio" $ radioEvs
    , fmapLabelShow "selected" $ fmap ((:[]) . fromEnum) (updated _toolWidgetConfig_tool)
    ]


  return ToolWidget {
    _toolWidget_setTool = setTool
  }

  {-
      keyPressEv k = flip push (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \vtyev -> do
        consuming <- sample _toolWidgetConfig_consumingKeyboard
        return $ if not consuming
          then case vtyev of
            V.EvKey (V.KChar k') [] | k' == k -> Just ()
            _                                 -> Nothing
          else Nothing
          -}
