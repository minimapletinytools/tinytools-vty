{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Params (
  ParamsWidgetConfig(..)
  , ParamsWidget(..)
  , holdParamsWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Flow.Vty.Manipulator.Types
import           Potato.Flow.Vty.PFWidgetCtx
import           Potato.Reflex.Vty.Helpers

import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Dependent.Sum                (DSum ((:=>)))
import qualified Data.IntMap                       as IM
import qualified Data.List.Extra                   as L
import qualified Data.Maybe
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as TZ
import           Data.Tuple.Extra

import qualified Graphics.Vty                      as V
import           Potato.Reflex.Vty.Widget.Layout
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty                        hiding (Constraint (..),
                                                    Orientation (..), col,
                                                    fixed, row, stretch, tile, TileConfig(..), Layout(..))




fixedNoFocus
  :: (Reflex t, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> VtyWidget t m a
  -> Layout t m a
fixedNoFocus sz = tile_ cfg . clickable where
  cfg = TileConfig {
      _tileConfig_constraint =  Constraint_Fixed <$> sz
      , _tile_Config_focusable = constDyn False
    }

-- | Default vty event handler for text inputs
updateTextZipperForSingleCharacter
  :: V.Event -- ^ The vty event to handle
  -> TZ.TextZipper -- ^ The zipper to modify
  -> TZ.TextZipper
updateTextZipperForSingleCharacter ev = case ev of
  V.EvKey (V.KChar '\t') [] -> id
  V.EvKey (V.KChar k) [] -> const $ TZ.top $ TZ.insertChar k TZ.empty
  V.EvKey V.KBS [] -> const TZ.empty
  V.EvKey V.KDel [] -> const TZ.empty
  V.EvKey (V.KChar 'u') [V.MCtrl] -> const TZ.empty
  _ -> id


paramsNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
paramsNavigation = do
  tabEv <- key (V.KChar '\t')
  returnEv <- key V.KEnter
  let fwd  = fmap (const 1) $ leftmost [tabEv, returnEv]
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

beginParamsLayout ::
  forall m t a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutReturnData t a)
  -> VtyWidget t m (Dynamic t (Maybe Int), a)
beginParamsLayout child = mdo
  navEv <- paramsNavigation
  let focusChildEv = layoutFocusEvFromNavigation navEv lrd
  lrd@LayoutReturnData {..} <- runIsLayoutVtyWidget child focusChildEv
  return (_layoutReturnData_focus, _layoutReturnData_value)

beginNoNavLayout ::
  forall m t a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutReturnData t a)
  -> VtyWidget t m (Dynamic t (Maybe Int), a)
beginNoNavLayout child = mdo
  LayoutReturnData {..} <- runIsLayoutVtyWidget child never
  return (_layoutReturnData_focus, _layoutReturnData_value)


-- Maybe Params stuff

-- | method type for picking out params from SuperSEltLabel
type ParamsSelector a = (Eq a) => SuperSEltLabel -> Maybe a

-- | method to extract common parameters from a selection
-- returns Nothing if nothing in the selection has the selected param
-- returns Just (selection, Nothing) if selection that has the selected param do not share the same value
selectParamsFromSelection :: (Eq a) => ParamsSelector a -> Selection -> Maybe (Selection, Maybe a)
selectParamsFromSelection ps selection = r where
  -- TODO don't do list conversion in between whataver ugh
  params = catMaybes . toList . fmap (\ssl@(rid,_,_) -> ps ssl >>= \a -> Just (ssl, a)) $ selection
  values = fmap snd params
  subSelection = Seq.fromList $ fmap fst params
  r = case values of
    [] -> Nothing
    x:xs -> if L.allSame values
      then Just (subSelection, Just x)
      else Just (subSelection, Nothing)


type MaybeParamsWidgetOutputDyn t m = Dynamic t (Maybe (VtyWidget t m (Dynamic t Int, Event t ControllersWithId)))

type MaybeParamsWidgetFn t m a = Dynamic t (Selection, Maybe a) -> MaybeParamsWidgetOutputDyn t m

-- |
-- returned Dynamic contains Nothing if selection was Nothing, otherwise contains Just the widget to modify parameters
holdMaybeParamsWidget :: forall t m a. (MonadWidget t m)
  => Dynamic t (Maybe (Selection, Maybe a)) -- ^ selection/params input
  -> MaybeParamsWidgetFn t m a -- ^ function creating widget, note that it should always return non-nothing but using Maybe type makes life easier
  -> VtyWidget t m (MaybeParamsWidgetOutputDyn t m)
holdMaybeParamsWidget mInputDyn widgetFn = do
  uniqDyn <- holdUniqDynBy (\a b -> isJust a == isJust b) mInputDyn
  return . join . ffor uniqDyn $ \case
    Nothing -> constDyn Nothing
    -- eh this is weird, maybe using fromJust is ok due to laziness but I don't care to find out
    Just _ -> widgetFn (fmap (fromMaybe (Seq.empty, Nothing)) mInputDyn)

emptyWidget :: (Monad m) => VtyWidget t m ()
emptyWidget = return ()

paramsLayout :: (MonadWidget t m) => Dynamic t [VtyWidget t m (Dynamic t Int, Event t ControllersWithId)] -> VtyWidget t m (Event t ControllersWithId)
paramsLayout widgets' = (switchHold never =<<) . networkView . ffor widgets' $ \widgets -> fmap snd $ beginNoNavLayout $ col $ do
  outputs <- forM widgets $ \w -> mdo
    (sz, ev) <- fixed sz w
    return ev
  return $ leftmostWarn "paramsLayout" outputs


-- SuperStyle stuff
data SuperStyleCell = SSC_TL | SSC_TR | SSC_BL | SSC_BR | SSC_V | SSC_H | SSC_Fill

updateFromSuperStyle :: SuperStyleCell -> (SuperStyle -> TZ.TextZipper)
updateFromSuperStyle ssc = TZ.fromText . T.singleton . gettfn ssc where
  gettfn SSC_TL = _superStyle_tl
  gettfn SSC_TR = _superStyle_tr
  gettfn SSC_BL = _superStyle_bl
  gettfn SSC_BR = _superStyle_br
  gettfn SSC_V = _superStyle_vertical
  gettfn SSC_H = _superStyle_horizontal
  gettfn SSC_Fill = (\case
    FillStyle_Simple c -> c) . _superStyle_fill

-- copy pasta from Reflex.Vty.Widget.Input.textInput a lot of stuff in here is not necessary
-- TODO DELETE simplify.. you don't need TextZipper
cellInput
  :: (Reflex t, MonadHold t m, MonadFix m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> VtyWidget t m (Dynamic t Text)
cellInput modifyEv c0 = mdo
  i <- input
  f <- focus
  dh <- displayHeight
  dw <- displayWidth
  rec v <- foldDyn ($) c0 $ mergeWith (.)
        [ fmap updateTextZipperForSingleCharacter i
        , modifyEv
        , let displayInfo = (,) <$> current rows <*> scrollTop
          in ffor (attach displayInfo click) $ \((dl, st), MouseDown _ (mx, my) _) ->
            TZ.goToDisplayLinePosition mx (st + my) dl
        ]
      click <- mouseDown V.BLeft
      let cursorAttrs = ffor f $ \x -> if x then cursorAttributes else V.defAttr
      let rows = (\w s c -> TZ.displayLines w V.defAttr c s)
            <$> dw
            <*> (TZ.mapZipper <$> (constDyn id) <*> v)
            <*> cursorAttrs
          img = images . TZ._displayLines_spans <$> rows
      y <- holdUniqDyn $ TZ._displayLines_cursorY <$> rows
      let newScrollTop :: Int -> (Int, Int) -> Int
          newScrollTop st (h, cursorY)
            | cursorY < st = cursorY
            | cursorY >= st + h = cursorY - h + 1
            | otherwise = st
      let hy = attachWith newScrollTop scrollTop $ updated $ zipDyn dh y
      scrollTop <- hold 0 hy
      tellImages $ (\imgs st -> (:[]) . V.vertCat $ drop st imgs) <$> current img <*> scrollTop
  return $ TZ.value <$> v

makeSuperStyleTextEntry :: (Reflex t, MonadHold t m, MonadFix m) => SuperStyleCell -> Dynamic t (Maybe SuperStyle) -> VtyWidget t m (Behavior t PChar)
makeSuperStyleTextEntry ssc mssDyn = do
  mss0 <- sample . current $ mssDyn
  let modifyEv = (fmap (maybe id (\ss -> const (updateFromSuperStyle ssc ss))) (updated mssDyn))
  ti <- cellInput modifyEv $ case mss0 of
    Nothing  -> ""
    Just ss0 -> updateFromSuperStyle ssc ss0
  return . current . fmap (\t -> maybe ' ' (\(c,_) -> c) (T.uncons t)) $ ti

makeSuperStyleEvent :: (Reflex t)
  => Behavior t PChar
  -> Behavior t PChar
  -> Behavior t PChar
  -> Behavior t PChar
  -> Behavior t PChar
  -> Behavior t PChar
  -> Behavior t PChar
  -> Event t ()
  -> Event t SuperStyle
makeSuperStyleEvent tl v bl h f tr br trig = pushAlways pushfn trig where
  pushfn _ = do
    tl' <- sample tl
    v' <- sample v
    bl' <- sample bl
    h' <- sample h
    f' <- sample f
    tr' <- sample tr
    br' <- sample br
    return $ def {
        _superStyle_tl    = tl'
        , _superStyle_tr     = tr'
        , _superStyle_bl        = bl'
        , _superStyle_br         = br'
        , _superStyle_vertical   = v'
        , _superStyle_horizontal = h'
        --, _superStyle_point      :: PChar
        , _superStyle_fill       = FillStyle_Simple f'
      }

holdSuperStyleWidget :: (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m) => MaybeParamsWidgetFn t m SuperStyle
holdSuperStyleWidget inputDyn = constDyn . Just $ mdo
  -- TODO need to ignore tab events or something
  let
    mssDyn = fmap snd inputDyn
    selectionDyn = fmap fst inputDyn
  -- TODO change this so it's left to right
  (focusDyn,(tl,v,bl,h,f,tr,br)) <- beginParamsLayout $ row $ do
    (tl'',v'',bl'') <- fixedL 1 $ col $ do
      tl' <- fixed 1 $ makeSuperStyleTextEntry SSC_TL mssDyn
      v' <- fixed 1 $ makeSuperStyleTextEntry SSC_V mssDyn
      bl' <- fixed 1 $ makeSuperStyleTextEntry SSC_BL mssDyn
      return (tl',v',bl')
    (h'',f'') <- fixedL 1 $ col $ do
      h' <- fixed 1 $ makeSuperStyleTextEntry SSC_H mssDyn
      f' <- fixed 1 $ makeSuperStyleTextEntry SSC_Fill mssDyn
      _ <- fixedNoFocus 1 $ emptyWidget
      return (h',f')
    (tr'',br'') <- fixedL 1 $ col $ do
      tr' <- fixed 1 $ makeSuperStyleTextEntry SSC_TR mssDyn
      _ <- fixedNoFocus 1 $ emptyWidget
      br' <- fixed 1 $ makeSuperStyleTextEntry SSC_BR mssDyn
      return (tr',br')
    return (tl'',v'',bl'',h'',f'',tr'',br'')
  let
    fmapfn ss (rid,_,seltl) = case getSEltLabelSuperStyle seltl of
      Nothing -> Nothing
      Just oldss -> if oldss == ss
        then Nothing
        else Just (rid, CTagSuperStyle :=> Identity (CSuperStyle (DeltaSuperStyle (oldss, ss))))
    fforfn (selection, ss) = case Data.Maybe.mapMaybe (fmapfn ss) . toList $ selection of
      [] -> Nothing
      x  -> Just $ IM.fromList x
    -- TODO maybe just do it when any of the cell dynamics are updated rather than when focus changes...
    outputEv = fforMaybe (attach (current selectionDyn) $ makeSuperStyleEvent tl v bl h f tr br (void $ updated focusDyn)) fforfn
  return (4, outputEv)





-- Text Alignment stuff
holdTextAlignmentWidget :: forall t m. (MonadWidget t m) => MaybeParamsWidgetFn t m TextAlign
holdTextAlignmentWidget taDyn = ffor taDyn $ \(selection, mta) -> Just $ do
  let
    startAlign = case mta of
      Nothing               -> []
      Just TextAlign_Left   -> [0]
      Just TextAlign_Center -> [1]
      Just TextAlign_Right  -> [2]

  setAlignmentEv' <- radioList (constDyn ["left","center","right"]) (constDyn startAlign)
  let
    setAlignmentEv = fmap (\case
        0 -> TextAlign_Left
        1 -> TextAlign_Center
        2 -> TextAlign_Right
      ) $ setAlignmentEv'
    pushAlignmentFn :: TextAlign -> PushM t (Maybe ControllersWithId)
    pushAlignmentFn ta = do
      let
        fmapfn (rid,_,seltl) = case getSEltLabelBoxTextStyle seltl of
          Nothing -> Nothing
          Just oldts -> if oldts == TextStyle ta
            then Nothing
            else Just (rid, CTagBoxTextStyle :=> Identity (CTextStyle (DeltaTextStyle (oldts, TextStyle ta))))
      return $ case Data.Maybe.mapMaybe fmapfn . toList $ selection of
        [] -> Nothing
        x  -> Just $ IM.fromList x
    alignmentParamsEv = push pushAlignmentFn setAlignmentEv
  return (1, alignmentParamsEv)

data SEltParams = SEltParams {
    --_sEltParams_sBox =
  }

data ParamsWidgetConfig t = ParamsWidgetConfig {
  _paramsWidgetConfig_pfctx              :: PFWidgetCtx t
}

data ParamsWidget t = ParamsWidget {
  _paramsWidget_paramsEvent :: Event t ControllersWithId
}

presetStyles = ["╔╗╚╝║═█","****|-*"]


holdParamsWidget :: forall t m. (MonadWidget t m)
  => ParamsWidgetConfig t
  -> VtyWidget t m (ParamsWidget t)
holdParamsWidget ParamsWidgetConfig {..} = do

  let
    selectionDyn = _goatWidget_selection (_pFWidgetCtx_goatWidget _paramsWidgetConfig_pfctx)

    textAlignSelector = (fmap (\(TextStyle ta) -> ta)) . getSEltLabelBoxTextStyle . thd3
    mTextAlignInputDyn = fmap ( selectParamsFromSelection textAlignSelector) selectionDyn

    mSuperStyleInputDyn = fmap (selectParamsFromSelection (getSEltLabelSuperStyle . thd3)) selectionDyn

  textAlignmentWidget <- holdMaybeParamsWidget mTextAlignInputDyn holdTextAlignmentWidget
  superStyleWidget2 <- holdMaybeParamsWidget mSuperStyleInputDyn holdSuperStyleWidget



  let
    superStyleWidget = do
      typeChoiceDyn <- radioListSimple 0 ["presets", "custom"]

      -- this is just a potato implementation to get us started
      setStyleEvEv <- networkView $ ffor typeChoiceDyn $ \case
        0 -> do
          setStyleEv' <- beginLayout $ col $ do
            fixed 1 emptyWidget -- just to make a space
            presetClicks <- forM presetStyles $ \s -> fixedL 1 $ row $ stretch $ do
              -- TODO highlight if style matches selection
              text (constant (T.pack s))
              fmap (fmap (\_ -> s)) (mouseDown V.BLeft)
            return $ fmap superStyle_fromListFormat (leftmost presetClicks)
          return setStyleEv'
        1 -> return never

      setStyleEv <- switchHold never setStyleEvEv

      let
        pushSuperStyleFn :: SuperStyle -> PushM t (Maybe ControllersWithId)
        pushSuperStyleFn ss = do
          selection <- sample . current $ _goatWidget_selection (_pFWidgetCtx_goatWidget _paramsWidgetConfig_pfctx)
          let
            fmapfn (rid,_,seltl) = case getSEltLabelSuperStyle seltl of
              Nothing -> Nothing
              Just oldss -> if oldss == ss
                then Nothing
                else Just (rid, CTagSuperStyle :=> Identity (CSuperStyle (DeltaSuperStyle (oldss, ss))))
          return $ case Data.Maybe.mapMaybe fmapfn . toList $ selection of
            [] -> Nothing
            x  -> Just $ IM.fromList x
        ssparamsEv = push pushSuperStyleFn setStyleEv
      return (4, ssparamsEv)

  -- do some magic to collapse MaybeParamsWidgetOutputDyn and render it with paramsLayout
  outputEv <- paramsLayout . fmap catMaybes . mconcat . (fmap (fmap (:[]))) $ [textAlignmentWidget, superStyleWidget2, constDyn $ Just superStyleWidget]

  return ParamsWidget {
    _paramsWidget_paramsEvent = traceEvent "wtf" $ outputEv
  }
