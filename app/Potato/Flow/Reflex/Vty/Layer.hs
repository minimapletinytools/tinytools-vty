
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
import qualified Data.Sequence                      as Seq
import qualified Data.Text                          as T
import           Data.Text.Zipper
import qualified Data.Text.Zipper                   as TZ
import           Data.These

import qualified Graphics.Vty                       as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

--moveChar :: Char
--moveChar = '≡'
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

{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y


-- | same as textInput but specialized for layers
layerEltTextInput
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Event t VtyEvent -- ^ input events filtered and modified for layers
  -> TextInputConfig t
  -> VtyWidget t m (TextInput t)
layerEltTextInput i cfg = mdo
  let
    f = constDyn True
  dh <- displayHeight
  dw <- displayWidth
  v <- foldDyn ($) (_textInputConfig_initialValue cfg) $ mergeWith (.)
    [ uncurry (updateTextZipperForLayers (_textInputConfig_tabWidth cfg)) <$> attach (current dh) i
    , _textInputConfig_modify cfg
    , let displayInfo = (,) <$> current rows <*> scrollTop
      in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
        goToDisplayLinePosition mx (st + my) dl
    ]
  click <- mouseDown V.BLeft
  let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
  -- TODO consider deleting multi-line support here..
  let rows = (\w s c -> displayLines w V.defAttr c s)
        <$> dw
        <*> (mapZipper <$> _textInputConfig_display cfg <*> v)
        <*> cursorAttrs
      img = images . _displayLines_spans <$> rows
  y <- holdUniqDyn $ _displayLines_cursorY <$> rows
  let newScrollTop :: Int -> (Int, Int) -> Int
      newScrollTop st (h, cursorY)
        | cursorY < st = cursorY
        | cursorY >= st + h = cursorY - h + 1
        | otherwise = st
  let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
  scrollTop <- hold 0 hy
  tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
  return $ TextInput
    { _textInput_value = value <$> v
    , _textInput_lines = length . _displayLines_spans <$> rows
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

data LEltState = LEltState {
  _lEltState_hidden          :: Bool
  , _lEltState_locked        :: Bool
  , _lEltState_contracted    :: Bool -- ^ only applies to folders
  , _lEltState_isFolderStart :: Bool
  , _lEltState_isFolderEnd   :: Bool
  , _lEltState_label         :: Text
  , _lEltState_layerPosition :: Maybe Int -- ^ Nothing if layer position is not known yet
  , _lEltState_selected      :: Bool

  , _lEltState_rEltId        :: Int -- ^ this is for debugging (ok apparantly, I need this to do renaming, just be mindful when serializing/deserializing this)
} deriving (Eq, Show)

holdLayerWidget :: forall t m. (Adjustable t m, PostBuild t m, NotReady t m,  MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
--holdLayerWidget _ = return $ LayerWidget never never never (constant False)
holdLayerWidget = holdLayerWidget'


holdLayerWidget' :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, NotReady t m,  MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget' LayerWidgetConfig {..} = do
  regionWidthDyn <- displayWidth
  regionHeightDyn <- displayHeight
  scrollEv <- mouseScroll
  inp <- input
  focusDyn <- focus

  let
    PFWidgetCtx {..} = _layerWidgetConfig_pfctx

    -- event that finalizes text entry in LayerElts
    finalizeSet :: Event t ()
    finalizeSet = flip fmapMaybe inp $ \case
      V.EvKey (V.KEnter) [] -> Just ()
      -- TODO this should not capture scroll events
      V.EvMouseDown _ _ _ _ -> Just ()
      _                     -> Nothing

    loseFocus = fmapMaybe (\x -> if not x then Just () else Nothing) $ updated focusDyn
    -- event tha tcancels text entry in LayerElts as well as cancel mouse drags
    cancelInput :: Event t ()
    cancelInput = leftmost [_pFWidgetCtx_ev_cancel, loseFocus]

    -- tracks if mouse was released anywhere including off pane (needed to cancel mouse drags)
    -- TODO we can get rid of this if we switch to pane 2
    mouseRelease :: Event t ()
    mouseRelease = flip fmapMaybe (_pFWidgetCtx_ev_input) $ \case
      V.EvMouseUp _ _ _ -> Just ()
      _                     -> Nothing

    padTop = 0
    padBottom = 0
    regionDyn = ffor2 regionWidthDyn regionHeightDyn (,)


    layerREltIdsDyn :: Dynamic t (Seq REltId)
    layerREltIdsDyn = _sEltLayerTree_view (_pfo_layers _pFWidgetCtx_pfo)

    -- TODO unify with the one in Canvas
    changesEv :: Event t (REltIdMap (Maybe SEltLabel))
    changesEv = fmap (fmap snd) $ _sEltLayerTree_changeView (_pfo_layers _pFWidgetCtx_pfo)

    selectedEv :: Event t ([(REltId, LayerPos)])
    selectedEv = updated
      $ fmap (fmap (\(rid,lp,_) -> (rid,lp)))
      $ fmap snd
      $ _selectionManager_selected _layerWidgetConfig_selectionManager

    deselectAll :: REltIdMap LEltState -> REltIdMap LEltState
    deselectAll = IM.map (\lelts -> lelts { _lEltState_selected = False } )
    -- TODO selected element should expand folder of its parents
    lEltStateMapDyn_foldfn_selected :: [(REltId, LayerPos)] -> REltIdMap LEltState -> REltIdMap LEltState
    lEltStateMapDyn_foldfn_selected ridlps old_leltsmap =
      foldr (\(rid,_) accm -> IM.adjust (\lelts -> lelts { _lEltState_selected = True }) rid accm) (deselectAll old_leltsmap) ridlps
    lEltStateMapDyn_foldfn_changes :: REltIdMap (Maybe SEltLabel) -> REltIdMap LEltState -> REltIdMap LEltState
    lEltStateMapDyn_foldfn_changes seltlmap old_leltsmap = new_leltsmap where
      only1_map_fn :: REltId -> Maybe SEltLabel -> LEltState
      only1_map_fn _ Nothing = error "expect deleted element to be  present in original map"
      -- TODO properly handle folders and whatever else needs to be added here
      only1_map_fn rid (Just (SEltLabel label _)) = LEltState False False False False False label Nothing False rid
      combineFn :: REltId -> Maybe SEltLabel -> LEltState -> Maybe LEltState
      combineFn _ Nothing _ = Nothing
      combineFn _ (Just (SEltLabel sname _)) lelts       = Just $ lelts { _lEltState_label = sname }
      new_leltsmap = IM.mergeWithKey combineFn (IM.mapWithKey only1_map_fn) id seltlmap old_leltsmap

    lEltStateMapDyn_foldfn :: These [(REltId, LayerPos)] (REltIdMap (Maybe SEltLabel)) -> REltIdMap LEltState -> REltIdMap LEltState
    lEltStateMapDyn_foldfn (This lps) = lEltStateMapDyn_foldfn_selected lps
    lEltStateMapDyn_foldfn (That seltlmap) = lEltStateMapDyn_foldfn_changes seltlmap
    lEltStateMapDyn_foldfn (These lps seltlmap) = lEltStateMapDyn_foldfn_selected lps . lEltStateMapDyn_foldfn_changes seltlmap


  -- TODO figure out how to do proper deserialization of this on load (main issue is that rEltIds need to be rematched based on layer position in the LEltState)
  -- TODO Consider switching this to use Incremental/mergeIntIncremental
  lEltStateMapDyn' :: Dynamic t (REltIdMap LEltState)
    <- foldDyn lEltStateMapDyn_foldfn IM.empty (align selectedEv changesEv)
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

    prepLayers_mapfn :: Seq LEltState -> Seq (Int, LEltState) -- first Int is indentation level
    prepLayers_mapfn leltss = r where
      prepLayers_foldfn :: (Int, Int, Seq (Int, LEltState)) -> Int -> LEltState -> (Int, Int, Seq (Int, LEltState))
      prepLayers_foldfn (ident, contr, acc) _ lelts = (newident, newcontr, acc Seq.|> (newident,lelts)) where
        -- TODO folders
        newident = ident
        newcontr = contr
      (_,_,r) = Seq.foldlWithIndex prepLayers_foldfn (0, 0, Seq.empty) leltss
    prepLayersDyn :: Dynamic t (Seq (Int, LEltState))
    prepLayersDyn = fmap prepLayers_mapfn lEltStateContractedList

    -- ::scrolling::
    -- TODO this seems to be cropping by one too much from the bottom
    -- I guess you can solvethis easily just by adding a certain padding allowance...
    maxScroll :: Behavior t Int
    maxScroll = current $ ffor2 lEltStateList regionDyn $ \leltss (_,h) -> max 0 (Seq.length leltss - h + padBottom)
    -- sadly, mouse scroll events are kind of broken, so you need to do some in between event before you can scroll in the other direction
    requestedScroll :: Event t Int
    requestedScroll = ffor scrollEv $ \case
      ScrollDirection_Up -> (-1)
      ScrollDirection_Down -> 1
    updateLine maxN delta ix = min (max 0 (ix + delta)) maxN
  -- TODO this doesn't pick up on latest maxScroll changes, not a big deal
  lineIndexDyn :: Dynamic t Int
    <- foldDyn (\(maxN, delta) ix -> updateLine maxN delta ix) 0 $
      attach maxScroll requestedScroll

  -- ::actually draw images::
  let
    makeLayerImage :: Int -> (Int, LEltState) -> V.Image
    makeLayerImage width (ident, LEltState {..}) = r where
      attr = if _lEltState_selected then lg_layer_selected else lg_default
      r = V.text' attr . T.pack . L.take width
        $ replicate ident ' '
        -- <> [moveChar]
        <> if' _lEltState_isFolderStart (if' _lEltState_contracted [expandChar] [closeChar]) []
        <> if' _lEltState_hidden [hiddenChar] [visibleChar]
        <> if' _lEltState_locked [lockedChar] [unlockedChar]
        <> " "
        <> show _lEltState_rEltId
        <> " "
        -- TODO render text zipper here instead
        <> T.unpack _lEltState_label
    layerImages :: Behavior t [V.Image]
    layerImages = current $ fmap ((:[]) . V.vertCat) $ ffor3 regionDyn lineIndexDyn prepLayersDyn $ \(w,h) li pl ->
      map (makeLayerImage w) . L.take (max 0 (h - padBottom)) . L.drop li $ toList pl
  tellImages layerImages

  -- ::input stuff::
  selectedDyn <- holdDyn [] selectedEv


  click <- singleClick V.BLeft >>= return . ffilter (\x -> not $ _singleClick_didDragOff x)
  --click <- mouseDown V.BLeft >>= return . fmap _mouseDown_coordinates
  let
    -- return value is (layer position, indentation, relative (x,y) of click, LEltState)
    layerMouse_pushfn :: (Int, Int) -> PushM t (Maybe (LayerPos, Int, (Int,Int), LEltState))
    layerMouse_pushfn (x',y') = do
      pl <- sample . current $ prepLayersDyn
      scrollPos <- sample . current $ lineIndexDyn
      let
        y = y' - padTop + scrollPos
        mselected = Seq.lookup y pl
      return $ case mselected of
        Nothing -> Nothing
        Just (ident, lelts@LEltState {..}) -> r where
          x = x' - ident
          r = case _lEltState_layerPosition of
            Nothing -> error "expected layer position to be set"
            Just lp -> Just (lp, ident, (x,y), lelts)

    -- ::click event for renaming::
    clickOnSelected_pushfn :: SingleClick -> PushM t (Maybe (Int, (Int,Int), LEltState))
    clickOnSelected_pushfn SingleClick {..} =  do
      let
        pos = _singleClick_coordinates
        nomods = null _singleClick_modifiers
      layerMouse_pushfn pos >>= \case
        Nothing -> return Nothing
        Just (_, ident, (x,y), lelts@LEltState {..}) -> if nomods && _lEltState_selected && x > 2
          then return $ Just (ident, (x,y), lelts)
          else return Nothing
    clickOnSelectedEv :: Event t (Int, (Int,Int), LEltState)
    clickOnSelectedEv = push clickOnSelected_pushfn click

    -- ::selecting::
    selectEv_pushfn :: SingleClick -> PushM t (Maybe [LayerPos])
    selectEv_pushfn SingleClick {..} =  do
      let
        pos = _singleClick_coordinates
        shiftClick = isJust $ find (==V.MShift) _singleClick_modifiers
      layerMouse_pushfn pos >>= \case
        Nothing -> return Nothing
        Just (lp, _, (x,_), _) -> if not shiftClick
          -- TODO ignore clicks on buttons
          then return $ Just [lp]
          else do
            currentSelection <- sample . current $ selectedDyn >>= return . map snd
            let
              (found,newSelection') = foldl' (\(found',ss) s -> if s == lp then (True, ss) else (found', s:ss)) (False,[]) currentSelection
              newSelection = if found then newSelection' else lp:newSelection'
            return $ Just newSelection
    selectEv = push selectEv_pushfn (difference click clickOnSelectedEv)


  let
    labelWidgetDynFoldFn :: Either () (Int, (Int,Int), LEltState) -> VtyWidget t m (Event t (ControllersWithId)) -> PushM t (VtyWidget t m (Event t (ControllersWithId)))
    labelWidgetDynFoldFn (Left _) _ = return (return never)
    labelWidgetDynFoldFn (Right (ident, (_,y_orig), LEltState{..})) _ = return $ do
      let
        yDyn = fmap (\scroll ->  y_orig  - scroll) lineIndexDyn
        -- convert relevant input
        labelInput = fforMaybe (attach (current yDyn) inp) $ \(y,mm) -> case mm of
          V.EvMouseDown x y btn' mods -> if x > 2
            then Just $ V.EvMouseDown (x-3) y btn' mods
            else Nothing
          V.EvKey k mods -> Just $ V.EvKey k mods
          _ -> Nothing
        paneRegionDyn = DynRegion (constDyn (ident+5)) yDyn (fmap (subtract ident) regionWidthDyn) 1
      t <- pane paneRegionDyn (constDyn False) $ do
        fill ' '
        layerEltTextInput labelInput $ TextInputConfig (fromText _lEltState_label) never 0 (constDyn id)
      return $ flip push finalizeSet $ \_ -> do
        newText <- sample . current $ _textInput_value t
        return $ if _lEltState_label /= newText
          then Just $ IM.singleton _lEltState_rEltId $ CTagRename :=> Identity (CRename (_lEltState_label, newText))
          else Nothing

  -- TODO this is soft breaking with tab character... (prob just disable tab navigation everywhere..)
  -- TODO finalizeSet needs to ignore cases where click on the textinput (maybe can ignore this using difference or see comments for finalizeSet)
  labelWidgetDyn :: Dynamic t (VtyWidget t m (Event t (ControllersWithId)))
    <- foldDynM labelWidgetDynFoldFn (return never)
      (alignEitherWarn "labelWidgetDyn input" (leftmostwarn "labelWidgetDyn cancel" [cancelInput, finalizeSet]) clickOnSelectedEv)
  changeNameEv <- networkView labelWidgetDyn >>= switchHold never


  -- TODO buttons at the bottom
  -- TODO you probably want to put panes or something idk...
  -- actually fixed takes a dynamic..

  vLayoutPad 10 $ debugStream [
    never
    --, fmapLabelShow "changeNameEv" $ changeNameEv
    --, fmapLabelShow "changes" $ changesEv
    --, fmapLabelShow "selected" $ selectedEv
    --, fmapLabelShow "click" $ click
    --, fmapLabelShow "lineIndex" $ updated lineIndexDyn
    --, fmapLabelShow "input" $ inp
    ]

  return LayerWidget {
    _layerWidget_select = selectEv
    , _layerWidget_changeName = changeNameEv
    , _layerWidget_move = never
    , _layerWidget_consumingKeyboard = constant False
  }
