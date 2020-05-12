{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Tools (
  Tool(..)
  , tool_cursorState
  , ToolWidgetConfig(..)
  , ToolWidget(..)
  , holdToolsWidget
) where

import           Relude

import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.CanvasPane
import           Potato.Flow.Reflex.Vty.PFWidgetCtx

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
        actives = sort actives'
        ifoldlfn (output, []) _ _ = (output, [])
        ifoldlfn (output, a:as) i (l,t) = if i == a
          then ((l,t,True):output, as)
          else ((l,t,False):output, as)
        (r,_) = L.ifoldl ifoldlfn ([],actives) bs
    makeImage :: ((Int,Int,Int), Text, Bool) -> V.Image
    makeImage ((x,y,_), t, selected) = V.translate x y $ V.text' attr c where
      attr = lg_default --if selected then lg_layer_selected else lg_default
      c = if selected then "{" <> t <> "}" else "[" <> t <> "]"
  tellImages $ fmap (fmap makeImage) $ current buttons
  return $ flip push mouseDownEv $ \(MouseDown _ (px,py) _) -> do
    bs <- sample . current $ buttons
    return $ L.ifindIndex (\_ ((x,y,l),_,_) -> py == y && px >= x && px < x+l) bs



data Tool = TSelect | TPan | TBox | TLine | TText deriving (Eq, Show)

tool_cursorState :: Tool -> CursorState
tool_cursorState TPan = CSPan
tool_cursorState TBox = CSBox
tool_cursorState _    = CSSelecting

data ToolWidgetConfig t = ToolWidgetConfig {
  _toolWidgetConfig_pfctx               :: PFWidgetCtx t
  , _toolWidgetConfig_setDefault        :: Event t ()
  , _toolWidgetConfig_consumingKeyboard :: Behavior t Bool
}

data ToolWidget t = ToolWidget {
  _toolWidget_tool :: Event t Tool
}


onlyIfBeh :: (Reflex t) => Event t a -> Behavior t Bool -> Event t a
onlyIfBeh ev beh = fmapMaybe (\(b,e) -> if b then Just e else Nothing) $ attach beh ev


holdToolsWidget :: forall t m. (PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => ToolWidgetConfig t
  -> VtyWidget t m (ToolWidget t)
holdToolsWidget ToolWidgetConfig {..} = row $ do

  let
    -- TODO active tool should use a different style, but that means every button needs its own config...
    -- or make your own button method
    bc = ButtonConfig (pure singleBoxStyle) (pure thickBoxStyle)
    --bc = ButtonConfig (pure roundedBoxStyle) (pure roundedBoxStyle)

  -- TODO tool highlighting based on dynTool
  selectB <- fixed 3 $ textButton bc "s"
  panB <- fixed 3 $ textButton bc "╬"
  boxB <- fixed 3 $ textButton bc "□"
  lineB <- fixed 3 $ textButton bc "/"
  textB <- fixed 3 $ textButton bc "T"

  --fixed 100 $ radioList (constDyn ["s","╬","□","/","T"]) (constDyn [])

  let
    allowKB = fmap not _toolWidgetConfig_consumingKeyboard
    keyPressEv' k = (flip fmapMaybe) (_pFWidgetCtx_ev_input _toolWidgetConfig_pfctx) $ \case
      V.EvKey (V.KChar k') [] | k' == k -> Just ()
      _ -> Nothing
    keyPressEv k = onlyIfBeh (keyPressEv' k) allowKB

  dynTool <- holdDyn TSelect $ leftmost
    [TSelect <$ leftmost
      [ selectB
      , onlyIfBeh (_pFWidgetCtx_ev_cancel _toolWidgetConfig_pfctx) allowKB
      , _toolWidgetConfig_setDefault
      , keyPressEv 'v']
    , TPan <$ leftmost [panB, keyPressEv ' ']
    , TBox <$ leftmost [boxB, keyPressEv 'b']
    , TLine <$ leftmost [lineB, keyPressEv 'l']
    , TText <$ leftmost [textB, keyPressEv 't']]

  return ToolWidget {
    _toolWidget_tool = updated dynTool
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
