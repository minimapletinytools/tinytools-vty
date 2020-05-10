
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Layer (
  LayerWidgetConfig(..)
  , LayerWidget(..)
  , holdLayerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import qualified Data.Map                           as M
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Data.Text.Zipper                   as TZ
import           Data.Tuple.Extra

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Vty

moveChar :: Char
moveChar = '≡'
hiddenChar :: Char
hiddenChar = '-'
visibleChar :: Char
visibleChar = 'e'
lockedChar :: Char
lockedChar = '@'
unlockedChar :: Char
unlockedChar = 'a'
expandChar :: Char
expandChar = '»'
closeChar :: Char
closeChar = '«'

updateTextZipperForLayers
  :: Int -- ^ Tab width
  -> Int -- ^ Page size
  -> V.Event -- ^ The vty event to handle
  -> TZ.TextZipper -- ^ The zipper to modify
  -> TZ.TextZipper
updateTextZipperForLayers _ _ ev = case ev of
  -- Regular characters
  V.EvKey (V.KChar k) []          -> TZ.insertChar k
  -- Deletion buttons
  V.EvKey V.KBS []                -> TZ.deleteLeft
  V.EvKey V.KDel []               -> TZ.deleteRight
  -- Key combinations
  V.EvKey (V.KChar 'u') [V.MCtrl] -> const TZ.empty
  V.EvKey (V.KChar 'w') [V.MCtrl] -> TZ.deleteLeftWord
  -- Arrow keys
  V.EvKey V.KLeft []              -> TZ.left
  V.EvKey V.KRight []             -> TZ.right
  V.EvKey V.KHome []              -> TZ.home
  V.EvKey V.KEnd []               -> TZ.end
  _                               -> id

-- | same as 'row' but with tab navigation disabled
layerRow
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> VtyWidget t m a
layerRow child = do
  runLayout (pure Orientation_Row) 0 never child


-- TODO list of issues
-- click on layer elt has many cases:
  -- hide/lock
  -- open/close folder
  -- rename elt
  -- click drag to move
  -- click once to select
-- how to do multiselect (can't use shift key in vty)
  -- probbaly just a multi select toggle button at the bottom
-- layer elements get recreated all the time, you need to store visibility/lock state in a external map

--you can make your own verison of pane that only passes on click events if focused?
  --i.e. click once to focus, click second time to edit?
  --or maybe use timer to simulate double click?

-- DESIGN
-- drag on top of element puts it ABOVE
-- open folders contain a drop area where you can drag stuff to put it inside at the end (actually just FolderEnd)
-- new folder button at the bottom

-- TODO folder option (need support for indentation, expand/contract folder, and hirerarchical visibility/lock)
-- TODO visibility/lock input/output
data LayerElementConfig t = LayerElementConfig {
  _layerElementConfig_cancel :: Event t () -- ^ this fires when we want to force text input to lose its input focus
  --, _layerElementConfig_consumingMouse :: Behavior t Bool -- ^ why do I need this again?
  , _layerElementConfig_text :: Dynamic t Text
}

data LayerElement t = LayerElement {
  _layerElement_onMovePressed             :: Event t () -- ^ fires when mouse is pressed over the move icon
  , _layerElement_onClick                 :: Event t () -- ^ fires when mouse is pressed and released anywhere over this element without moving anywhere else
  , _layerElement_onRelease               :: Event t () -- ^ fires when mouse is released anywhere over the element
  , _layerElement_text                    :: Behavior t Text
  , _layerElementConfig_consumingKeyboard :: Behavior t Bool
}

holdLayerElement :: (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerElementConfig t
  -> VtyWidget t m (LayerElement t)
holdLayerElement LayerElementConfig {..} = do

  text0 <- sample . current $ _layerElementConfig_text

  let
    modifyLayerTextEv = fmap (\t -> const (TZ.fromText t)) $ updated _layerElementConfig_text
    layerTextInputCfg = TextInputConfig {
        _textInputConfig_initialValue = TZ.fromText text0
        , _textInputConfig_modify = modifyLayerTextEv
        , _textInputConfig_tabWidth = 0
        , _textInputConfig_display = constDyn id
      }

  click <- singleClick V.BLeft
  mouseUp <- mouseUp
  (movePressed, textInput) <- layerRow $ do

    movePressed' <- fixed 1 $ do
      text $ constant (T.singleton moveChar)
      void <$> mouseDown V.BLeft

    fixed 1 $ return ()
    fixed 1 $ return ()
    -- TODO need to create a pane that only passes mouse input if focused to allow click twice to edit text
    -- or make a textInput field that requires 2 clicks to enter edit mode (and listens to cancel events to lose "focus")
    text' <- stretch $ textInput layerTextInputCfg
    return (movePressed', current $ _textInput_value text')

  return
    LayerElement {
      _layerElement_onMovePressed = movePressed
      , _layerElement_onClick = void click
      , _layerElement_onRelease = void mouseUp
      , _layerElement_text                    = textInput
      -- TODO true when "focused"
      , _layerElementConfig_consumingKeyboard = constant False
    }



data LayerWidgetConfig t = LayerWidgetConfig {
  _layerWidgetConfig_pfctx              :: PFWidgetCtx t
  , _layerWidgetConfig_temp_sEltTree    :: Dynamic t [SuperSEltLabel]
  , _layerWidgetConfig_selectionManager :: SelectionManager t
}

data LayerWidget t = LayerWidget {
  _layerWidget_select              :: Event t [LayerPos]
  , _layerWidget_changeName        :: Event t ControllersWithId
  , _layerWidget_consumingKeyboard :: Behavior t Bool
}

holdLayerWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget LayerWidgetConfig {..} = do
  let
    -- TODO don't listen to global events
    -- instead maybe listen to local events and also listen to lose focus event, or maybe that's a cancel?
    finalizeSet :: Event t ()
    finalizeSet = flip fmapMaybe (_pFWidgetCtx_ev_input _layerWidgetConfig_pfctx) $ \case
      V.EvKey (V.KEnter) [] -> Just ()
      V.EvMouseDown _ _ _ _ -> Just ()
      _                     -> Nothing


  --pw <- displayWidth
  --ph <- displayHeight
  (switchClicks, consuming) <- col $ mdo
    --fixed 1 $ text . current . fmap (show . length)$ _layerWidgetConfig_temp_sEltTree
    fixed 3 $ debugStream [never
      --, fmapLabelShow "click" $ switchDyn switchClicks'
      ]

    let
      layermap :: Dynamic t (Map LayerPos SuperSEltLabel)
      layermap = fmap M.fromList
        $ fmap (fmap (\sseltl@(_,lp,_) -> (lp, sseltl)))
        $ fmap toList _layerWidgetConfig_temp_sEltTree

    lelts' <- stretch $ col $ list layermap $ \sseltlDyn -> do
      fixed 1 $ do
        --text $ current $ fmap (\(rid,lp,seltl) -> show rid <> " " <> _sEltLabel_name seltl) sseltlDyn
        --void <$> mouseDown V.BLeft
        holdLayerElement LayerElementConfig {
            _layerElementConfig_cancel = finalizeSet
            , _layerElementConfig_text = fmap (\(rid,lp,seltl) -> show rid <> " " <> _sEltLabel_name seltl) sseltlDyn
          }
    let
      lelts = fmap M.toList lelts'
      -- TODO lol
      --leltsonly = fmap  (fmap (\(_,e) -> e)) lelts
      --consuming' = fmap (foldr (\b1 b2 -> ffor2 b1 b2 (||)) (constant False) $ fmap _layerElementConfig_consumingKeyboard) leltsonly
      consuming' = constant False
      switchClicks' = fmap (leftmost . fmap (\(i,e) -> _layerElement_onClick e $> i)) lelts

    return (switchClicks', consuming')
  return LayerWidget {
    _layerWidget_select = fmap (:[]) $ switchDyn switchClicks
    , _layerWidget_changeName = never
    , _layerWidget_consumingKeyboard = consuming
  }

{-
-- currently remakes entire layer view each time anything changes
-- TODO use holdUniqDyn so it only happens when things actually change
labels :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  -> Event t Selected -- ^ selection event
  -> VtyWidget t m (Dynamic t (Map Int (TodoOutput t)))
labels selectionEv = mdo
  tabNav <- tabNavigation
  let insertNav = 1 <$ insert
      nav = leftmost [tabNav, insertNav]
      tileCfg = def { _tileConfig_constraint = pure $ Constraint_Fixed 1}
  listOut <- runLayout (pure Orientation_Column) 0 nav $
    listHoldWithKey todosMap0 updates $ \k t -> tile tileCfg $ do
      selecteMe <- void <$> mouseDown V.BLeft
      -- TODO only focus if selected
      focusMe <- never -- void <$> mouseDown V.BLeft
      r <- todo t
      return (focusMe, r)
  let delete = ffor todoDelete $ \k -> Map.singleton k Nothing
      updates = leftmost [insert, delete]
      todoDelete = switch . current $
        leftmost .  Map.elems . Map.mapWithKey (\k -> (k <$) . _todoOutput_delete) <$> listOut
      todosMap = joinDynThroughMap $ fmap _todoOutput_todo <$> listOut
      insert = ffor (tag (current todosMap) newTodo) $ \m -> case Map.lookupMax m of
        Nothing     -> Map.singleton 0 $ Just $ Todo "" False
        Just (k, _) -> Map.singleton (k+1) $ Just $ Todo "" False
      selectOnDelete = fanMap $ (`Map.singleton` ()) <$> attachWithMaybe
        (\m k -> let (before, after) = Map.split k m
                  in  fmap fst $ Map.lookupMax before <|> Map.lookupMin after)
        (current todosMap)
        todoDelete
  return listOut

data LableOutputConfig t = LableOutput {

}

data LableOutput t = LableOutput {
  _labelOutput_changeName :: Event t Text
  , _labelOutput_show :: Dynamic t Bool
  , _labelOutput_lock :: Dynamic t Bool
}

label
  :: (MonadHold t m, MonadFix m, Reflex t, MonadNodeId m)
  => SuperSEltLabel
  -> VtyWidget t m ( t)
label (rid, lp, SEltLabel name _) _ = mdo

  dynShow <- holdDyn True $ leftmost [checkShowEv]
  (checkShowEv, checkLockEv) <- row $ do
    checkShowEv' <- fixed 3 $ checkbox def
    checkLockEv' <- fixed 3 $ checkbox def
    -- TODO proper check
    _ <- stretch $ do
      i <- input
      let
        -- TODO need to setup reader to pass in off-pane click events
        confirm <- fmap (filterKeyEv V.KRet) inp
        cancel <-  fmap (filterKeyEv V.KEsc) inp
      v <- textInput $ def { _textInputConfig_initialValue = TZ.fromText name }
      return v
  return $ TodoOutput
    { _todoOutput_todo = Todo <$> _textInput_value ti <*> value
    , _todoOutput_delete = d
    , _todoOutput_height = _textInput_lines ti
    }
  where
    filterKeyEv k = \case
      V.EvKey k _ -> Just ()
      _ -> Nothing
-}
