
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Reflex.Vty.Layer (
  LayerWidgetConfig(..)
  , LayerWidget(..)
  , holdLayerWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Reflex.Vty.Attrs
import           Potato.Flow.Reflex.Vty.PFWidgetCtx
import           Potato.Flow.Reflex.Vty.Selection
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget


import           Control.Monad.Fix
import           Data.Align
import           Data.Dependent.Sum                 (DSum ((:=>)))
import qualified Data.IntMap.Strict                 as IM
import qualified Data.List                          as L
import qualified Data.Map                           as M
import           Data.Maybe                         (fromJust)
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import qualified Data.Text.Zipper                   as TZ
import           Data.These
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
  _layerElementConfig_rEltId     :: REltId -- ^ just for convenience
  -- TODO
  --_layerElementConfig_cancel :: Event t () -- ^ this fires when we want to cancel text input
  , _layerElementConfig_finalize :: Event t () -- ^ this fires when we want to force text input to lose its input focus
  --, _layerElementConfig_consumingMouse :: Behavior t Bool -- ^ why do I need this again?
  -- dynamic is prob not necessary, I think this whole widget gets recreated anytime there are changes...
  , _layerElementConfig_text     :: Dynamic t Text
}

data LayerElement t = LayerElement {
  _layerElement_rEltId                    :: REltId -- ^ just for convenience
  , _layerElement_onMovePressed           :: Event t () -- ^ fires when mouse is pressed over the move icon
  , _layerElement_onClick                 :: Event t () -- ^ fires when mouse is pressed and released anywhere over this element without moving anywhere else
  , _layerElement_onRelease               :: Event t () -- ^ fires when mouse is released anywhere over the element
  , _layerElement_text                    :: Event t (Text, Text) -- ^ fires when _layerElementConfig_finalize fires IF there were any changes to the text
  , _layerElementConfig_consumingKeyboard :: Behavior t Bool
}

holdLayerElement :: (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerElementConfig t
  -> VtyWidget t m (LayerElement t)
holdLayerElement LayerElementConfig {..} = do

  text0 <- sample . current $ _layerElementConfig_text

  let
    -- pretty sure updated _layerElementConfig_text is not necessary, see comments in LayerElementConfig
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
      dragEv <- drag2 V.BLeft
      return $ void $ ffilter (\x -> _drag2_state x == DragStart) dragEv
    --fixed 1 $ return ()
    --fixed 1 $ return ()
    fixed 2 $ text (constant (show _layerElementConfig_rEltId))
    -- TODO need to create a pane that only passes mouse input if focused to allow click twice to edit text
    -- or make a textInput field that requires 2 clicks to enter edit mode (and listens to cancel events to lose "focus")
    text' <- stretch $ textInput layerTextInputCfg
    return (movePressed', current $ _textInput_value text')

  let
    textChangeEv = flip push _layerElementConfig_finalize $ \_ -> do
      prevText <- sample . current $ _layerElementConfig_text
      newText <- sample textInput
      return $ if prevText /= newText
        then Just (prevText, newText)
        else Nothing


  return
    LayerElement {
      _layerElement_rEltId = _layerElementConfig_rEltId
      , _layerElement_onMovePressed = movePressed
      , _layerElement_onClick = void click
      , _layerElement_onRelease = void mouseUp
      , _layerElement_text                    = textChangeEv
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
  -- TODO expand to support multi-move
  , _layerWidget_move              ::Event t (LayerPos, LayerPos)
  , _layerWidget_consumingKeyboard :: Behavior t Bool
}

holdLayerWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget = holdLayerWidgetNEW
--holdLayerWidget = holdLayerWidgetOld
--holdLayerWidget = holdLayerWidgetNothing

-- for testing performance
holdLayerWidgetNothing :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidgetNothing _ = return $ LayerWidget never never never (constant False)


-- old version, doesn't do much but doesn't go super slow
holdLayerWidgetOld :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidgetOld LayerWidgetConfig {..} = do
  --pw <- displayWidth
  --ph <- displayHeight
  switchClicks <- col $ mdo
    --fixed 1 $ text . current . fmap (show . length)$ _layerWidgetConfig_temp_sEltTree
    fixed 3 $ debugStream [never
      --, fmapLabelShow "click" $ switchDyn switchClicks'
      ]
    -- NOTE this is only possible because you added PostBuild to Layout
    clicks <- stretch $ col $ simpleList _layerWidgetConfig_temp_sEltTree $ \sseltl -> do
      fixed 1 $ do
        text $ current $ fmap (_sEltLabel_name . thd3) sseltl
        void <$> mouseDown V.BLeft
    let
      switchClicks' = fmap (leftmost . zipWith (\i c -> c $> i) [0..]) clicks
    return switchClicks'
  return LayerWidget {
    _layerWidget_select = fmap (:[]) $ switchDyn switchClicks
    , _layerWidget_changeName = never
    , _layerWidget_move              = never
    , _layerWidget_consumingKeyboard = constant False
  }


-- this version has serious performance issues, probably due to all the mouse events being listened to
holdLayerWidgetNew :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidgetNew LayerWidgetConfig {..} = do
  let
    -- TODO don't listen to global events
    -- instead maybe listen to local events and also listen to lose focus event, or maybe that's a cancel?
    -- event that finalizes text entry in LayerElts
    finalizeSet :: Event t ()
    finalizeSet = flip fmapMaybe (_pFWidgetCtx_ev_input _layerWidgetConfig_pfctx) $ \case
      V.EvKey (V.KEnter) [] -> Just ()
      V.EvMouseDown _ _ _ _ -> Just ()
      _                     -> Nothing
    -- event tha tcancels text entry in LayerElts as well as cancel mouse drags
    cancelInput :: Event t ()
    cancelInput = leftmost [_pFWidgetCtx_ev_cancel _layerWidgetConfig_pfctx]
    -- tracks if mouse was released anywhere including off pane (needed to cancel mouse drags)
    -- TODO we can get rid of this if we switch to pane 2
    mouseRelease :: Event t ()
    mouseRelease = flip fmapMaybe (_pFWidgetCtx_ev_input _layerWidgetConfig_pfctx) $ \case
      V.EvMouseUp _ _ _ -> Just ()
      _                     -> Nothing

  --pw <- displayWidth
  --ph <- displayHeight
  (clicks, move, rename, consuming) <- col $ mdo
    --fixed 1 $ text . current . fmap (show . length)$ _layerWidgetConfig_temp_sEltTree
    fixed 3 $ debugStream [never
        , fmapLabelShow "move" $ move'
        , fmapLabelShow "down" $ updated downPos
        , fmapLabelShow "up" $ releaseOn
      --, fmapLabelShow "click" $ switchDyn clicks'
      ]

    let
      layermap :: Dynamic t (Map LayerPos SuperSEltLabel)
      layermap = fmap M.fromList
        $ fmap (fmap (\sseltl@(_,lp,_) -> (lp, sseltl)))
        $ fmap toList _layerWidgetConfig_temp_sEltTree

    lelts' <- stretch $ col $ list layermap $ \sseltlDyn -> do
      -- gets recreated each time sseltlDyn changes so safe to sample here
      -- TODO this is broken, always 0 for some reason, makes no sense
      rid <- sample . current $ fmap (\(rid,_,_) -> rid) sseltlDyn
      fixed 1 $ do
        holdLayerElement LayerElementConfig {
            _layerElementConfig_rEltId = rid
            , _layerElementConfig_finalize = finalizeSet
            , _layerElementConfig_text = fmap (\(_,_,seltl) ->_sEltLabel_name seltl) sseltlDyn
          }
    dummyRelease <- fixed 1 $ mouseUp

    let
      lelts = fmap M.toList lelts'
      movePress = switchDyn $ fmap (leftmost . fmap (\(i,e) -> _layerElement_onMovePressed e $> i)) lelts
      releaseOn' = switchDyn $ fmap (leftmost . fmap (\(i,e) -> _layerElement_onRelease e $> i)) lelts
      releaseOn = leftmost [releaseOn', (fmap length (tag (current lelts) dummyRelease))]
      clicks' = switchDyn $ fmap (leftmost . fmap (\(i,e) -> _layerElement_onClick e $> i)) lelts

      -- TODO fix this lol
      --leltsonly = fmap  (fmap (\(_,e) -> e)) lelts
      --consuming' = fmap (foldr (\b1 b2 -> ffor2 b1 b2 (||)) (constant False) $ fmap _layerElementConfig_consumingKeyboard) leltsonly
      consuming' = constant False

      movePosFoldFn :: These Int () -> Int -> Int
      movePosFoldFn (This lp) _      = lp
      movePosFoldFn _              _ = -1

    -- track which elt we clicked down on
    downPos <- foldDyn movePosFoldFn (-1) $ align movePress (leftmost [cancelInput, difference mouseRelease movePress])
    let
      move' = flip push releaseOn $ \dst -> do
        src <- sample . current $ downPos
        return $ if src == -1
          then Nothing
          else Just $ (src, dst)

    let
      textChange' = switchDyn $ fmap (leftmost . fmap (\(i,e) -> fmap (\t -> (i,_layerElement_rEltId e,t)) (_layerElement_text e))) lelts
      nameChangeFn :: (LayerPos, REltId, (Text, Text)) -> ControllersWithId
      nameChangeFn (lp, rid, dt) = IM.singleton rid $ CTagRename :=> Identity (CRename dt)
      rename' = fmap nameChangeFn textChange'

    return (clicks', move', rename', consuming')

  return LayerWidget {
    -- TODO this is also kind of broken until you switch to pane2
    _layerWidget_select = fmap (:[]) $ clicks
    -- TODO THIS DOES NOT WORK RIGHT NOW, REltId IS SET INCORRECTLY
    , _layerWidget_changeName = rename
    -- TODO this is super broken until you switch to pane2
    , _layerWidget_move = move
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





{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


data LEltState = LEltState {
  _lEltState_hidden          :: Bool
  , _lEltState_locked        :: Bool
  , _lEltState_contracted    :: Bool -- ^ only applies to folders
  , _lEltState_isFolderStart :: Bool
  , _lEltState_isFolderEnd   :: Bool
  , _lEltState_label         :: Text
  , _lEltState_layerPosition :: Maybe Int -- ^ Nothing if layer position is not known yet
  , _lEltState_selected      :: Bool
} deriving (Eq, Show)


holdLayerWidgetNEW :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidgetNEW LayerWidgetConfig {..} = do
  regionWidthDyn <- displayWidth
  regionHeightDyn <- displayHeight
  scrollEv <- mouseScroll
  inp <- input
  let
    padTop = 0
    padBottom = 3

    regionDyn = ffor2 regionWidthDyn regionHeightDyn (,)
    PFWidgetCtx {..} = _layerWidgetConfig_pfctx

    layerREltIdsDyn :: Dynamic t (Seq REltId)
    layerREltIdsDyn = _sEltLayerTree_view (_pfo_layers _pFWidgetCtx_pfo)

    -- TODO unify with the one in Canvas
    changesEv :: Event t (REltIdMap (Maybe SEltLabel))
    changesEv = fmap (fmap snd) $ _sEltLayerTree_changeView (_pfo_layers _pFWidgetCtx_pfo)

    lEltStateMapDyn_foldfn :: REltIdMap (Maybe SEltLabel) -> REltIdMap LEltState -> REltIdMap LEltState
    lEltStateMapDyn_foldfn seltlmap old_leltsmap = new_leltsmap where
      only1_map_fn :: Maybe SEltLabel -> LEltState
      only1_map_fn Nothing = error "expect deleted element to be  present in original map"
      -- TODO properly handle folders and whatever else needs to be added here
      only1_map_fn (Just (SEltLabel label _)) = LEltState False False False False False label Nothing False
      combineFn :: REltId -> Maybe SEltLabel -> LEltState -> Maybe LEltState
      combineFn rid Nothing lelts = Nothing
      combineFn rid _ lelts       = Just lelts
      new_leltsmap = IM.mergeWithKey combineFn (IM.map only1_map_fn) id seltlmap old_leltsmap

  -- TODO Consider switching this to use Incremental/mergeIntIncremental
  lEltStateMapDyn' :: Dynamic t (REltIdMap LEltState)
    <- foldDyn lEltStateMapDyn_foldfn IM.empty changesEv
  lEltStateMapDyn <- holdUniqDyn lEltStateMapDyn'

  let
    lEltStateList_mapfn leltsmap lp rid = case IM.lookup rid leltsmap of
      Nothing -> error $ "expected to find " <> show rid <> " in lEltStateMapDyn"
      Just lelts -> lelts { _lEltState_layerPosition = Just lp }
    -- TODO rather than remaking this for every change, we should only do this if there are topology changes, and do per element updates otherwise
    lEltStateList :: Dynamic t (Seq LEltState)
    lEltStateList = ffor2 lEltStateMapDyn layerREltIdsDyn $ \leltsmap rids -> Seq.mapWithIndex (lEltStateList_mapfn leltsmap) rids

    -- TODO folder contraction
    lEltStateContractedList :: Dynamic t (Seq LEltState)
    lEltStateContractedList = lEltStateList

    -- TODO change this to Seq
    prepLayers_mapfn :: Seq LEltState -> Seq (Int, LEltState) -- first Int is indentation level
    prepLayers_mapfn leltss = r where
      prepLayers_foldfn :: (Int, Int, Seq (Int, LEltState)) -> Int -> LEltState -> (Int, Int, Seq (Int, LEltState))
      prepLayers_foldfn (ident, contr, acc) _ lelts = r where
        -- TODO folders
        newident = ident
        newcontr = contr
        r = (newident, newcontr, acc Seq.|> (newident,lelts))
      (_,_,r) = Seq.foldlWithIndex prepLayers_foldfn (0, 0, Seq.empty) leltss
    prepLayersDyn :: Dynamic t (Seq (Int, LEltState))
    prepLayersDyn = fmap prepLayers_mapfn lEltStateContractedList

    -- TODO fix ATTR
    makeImage :: Int -> (Int, LEltState) -> V.Image
    makeImage width (ident, LEltState {..}) = V.text' lg_default . T.pack . L.take width
      $ replicate ident ' '
      -- <> [moveChar]
      <> if' _lEltState_isFolderStart (if' _lEltState_contracted [expandChar] [closeChar]) []
      <> if' _lEltState_hidden [hiddenChar] [visibleChar]
      <> if' _lEltState_locked [lockedChar] [unlockedChar]
      <> " "
      <> show (fromJust _lEltState_layerPosition)
      <> " "
      <> T.unpack _lEltState_label

    -- TODO thisseems to be cropping by one from the bottom
    -- I guess you can solvethis easily just by adding a certain padding allowance...
    maxScroll :: Behavior t Int
    maxScroll = current $ ffor2 lEltStateList regionDyn $ \leltss (_,h) -> max 0 (Seq.length leltss - h - padBottom)


    -- sadly, mouse scroll events are kind of broken, so you need to do some in between event before you can scroll in the other direction
    requestedScroll :: Event t Int
    requestedScroll = ffor scrollEv $ \case
      ScrollDirection_Up -> (-1)
      ScrollDirection_Down -> 1
    updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
  lineIndexDyn :: Dynamic t Int
    <- foldDyn (\(maxN, delta) ix -> updateLine (maxN - 1) delta ix) 0 $
      attach maxScroll requestedScroll

  let
    images :: Behavior t [V.Image]
    images = current $ fmap ((:[]) . V.vertCat) $ ffor3 regionDyn lineIndexDyn prepLayersDyn $ \(w,h) li pl ->
      map (makeImage w) . L.take (max 0 (h - padBottom)) . L.drop li $ toList pl
  tellImages images

  -- input stuff
  click <- singleClick V.BLeft
  --let
    --selectEv_fmapfn :: ([(Int, LEltState)], )
    --selecattach (current prepLayersDyn) click

  -- TODO buttons at the bottom
  -- TODO you probably want to put panes or something idk...
  -- actually fixed takes a dynamic..

  debugStream [
    never
      --, fmapLabelShow "input" $ inp
    ]

  return LayerWidget {
    _layerWidget_select = never
    , _layerWidget_changeName = never
    , _layerWidget_move = never
    , _layerWidget_consumingKeyboard = constant False
  }
