
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


{-# INLINE if' #-}
if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y




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
  , _lEltState_rEltId        :: Int -- ^ this is for debugging
} deriving (Eq, Show)


holdLayerWidget :: forall t m. (Reflex t, Adjustable t m, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m)
  => LayerWidgetConfig t
  -> VtyWidget t m (LayerWidget t)
holdLayerWidget LayerWidgetConfig {..} = do
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
      only1_map_fn :: REltId -> Maybe SEltLabel -> LEltState
      only1_map_fn _ Nothing = error "expect deleted element to be  present in original map"
      -- TODO properly handle folders and whatever else needs to be added here
      only1_map_fn rid (Just (SEltLabel label _)) = LEltState False False False False False label Nothing False rid
      combineFn :: REltId -> Maybe SEltLabel -> LEltState -> Maybe LEltState
      combineFn rid Nothing lelts = Nothing
      combineFn rid _ lelts       = Just lelts
      new_leltsmap = IM.mergeWithKey combineFn (IM.mapWithKey only1_map_fn) id seltlmap old_leltsmap

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
      <> show _lEltState_rEltId
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
  click <- singleClick V.BLeft >>= return . fmap _singleClick_coordinates . ffilter (\x -> not $ _singleClick_didDragOff x)
  --click <- mouseDown V.BLeft >>= return . fmap _mouseDown_coordinates
  let
    selectEv_pushfn :: (Int, Int) -> PushM t (Maybe LayerPos)
    selectEv_pushfn (x',y') = do
      pl <- sample . current $ prepLayersDyn
      scrollPos <- sample . current $ lineIndexDyn
      let
        y = y' - padTop
        mselected = Seq.lookup y pl
      return $ case mselected of
        Nothing -> Nothing
        Just (ident, LEltState {..}) -> r where
          -- TODO ignore clicks on buttons
          x = x' - ident
          -- TODO assert here to make sure it's not a Nothing
          r = _lEltState_layerPosition

    -- TODO shift select by adding to current selection
    selectEv = fmap (:[]) $ push selectEv_pushfn click

  -- TODO buttons at the bottom
  -- TODO you probably want to put panes or something idk...
  -- actually fixed takes a dynamic..

  debugStream [
    never
    --, fmapLabelShow "click" $ click
    --, fmapLabelShow "input" $ inp
    ]

  return LayerWidget {
    _layerWidget_select = selectEv
    , _layerWidget_changeName = never
    , _layerWidget_move = never
    , _layerWidget_consumingKeyboard = constant False
  }
