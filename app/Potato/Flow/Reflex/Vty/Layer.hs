{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Layer (
  LayerWidgetConfig(..)
  , LayerWidget(..)
  , holdLayerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Data.Tuple.Extra

import           Reflex
import           Reflex.Vty



data LayerWidgetConfig t = LayerWidgetConfig {
  _layerWidgetConfig_temp_sEltTree      :: Dynamic t [SuperSEltLabel]
  , _layerWidgetConfig_selectionManager :: SelectionManager t
}

data LayerWidget t = LayerWidget {
  _layerWidget_select       :: Event t LayerPos
  , _layerWidget_changeName :: Event t ControllersWithId
}

holdLayerWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget LayerWidgetConfig {..} = do
  --pw <- displayWidth
  --ph <- displayHeight
  col $ do
    fixed 1 $ debugFocus
    fixed 1 $ text . current . fmap (show . length)$ _layerWidgetConfig_temp_sEltTree
    --addButton <- fixed 3 $ textButtonStatic def "add"
    -- note this is only possible because you added PostBuild to Layout
    stretch $ col $ simpleList _layerWidgetConfig_temp_sEltTree $ \sseltl -> do
      fixed 1 $ text $ current $ fmap (_sEltLabel_name . thd3) sseltl
  return LayerWidget {
    _layerWidget_select = never
    , _layerWidget_changeName = never
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
