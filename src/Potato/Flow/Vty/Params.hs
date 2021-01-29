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
import qualified Data.Maybe
import qualified Data.Text                         as T
import qualified Data.Text.Zipper as TZ
import qualified Data.List.Extra as L
import Data.Tuple.Extra

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Vty hiding (row, col, fixed, stretch, tile, Orientation (..), Constraint (..))
import Potato.Reflex.Vty.Widget.Layout2
import           Reflex.Potato.Helpers



paramsNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
paramsNavigation = do
  tabEv <- key (V.KChar '\t')
  returnEv <- key V.KEnter
  let fwd  = fmap (const 1) $ leftmost [tabEv, returnEv]
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

beginParamsLayout ::
  forall m t a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
  -> VtyWidget t m (Dynamic t (Maybe Int), a)
beginParamsLayout child = mdo
  navEv <- paramsNavigation
  let focusChildEv = fmap (\(mcur, shift) -> maybe (Just 0) (\cur -> Just $ (shift + cur) `mod` totalKiddos) mcur) (attach (current indexDyn) navEv)
  (_, indexDyn, totalKiddos, a) <- runIsLayoutVtyWidget child focusChildEv
  return (indexDyn, a)

beginNoNavLayout ::
  forall m t a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t (Maybe Int), Int, a)
  -> VtyWidget t m (Dynamic t (Maybe Int), a)
beginNoNavLayout child = mdo
  (_, indexDyn, totalKiddos, a) <- runIsLayoutVtyWidget child never
  return (indexDyn, a)


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

makeSuperStyleTextEntry :: (Reflex t, MonadHold t m, MonadFix m) => SuperStyleCell -> Dynamic t SuperStyle -> VtyWidget t m (Behavior t PChar)
makeSuperStyleTextEntry ssc ssDyn = do
  ss0 <- sample . current $ ssDyn
  let config = def {
      _textInputConfig_initialValue = updateFromSuperStyle ssc ss0
      , _textInputConfig_modify = fmap (\ss -> const (updateFromSuperStyle ssc ss)) (updated ssDyn)
    }
  ti <- textInput config
  return . current . fmap (\t -> maybe ' ' (\(c,_) -> c) (T.uncons t)) $ _textInput_value ti

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

holdSuperStyleWidget :: (Reflex t, PostBuild t m, MonadHold t m, MonadFix m, MonadNodeId m) => Dynamic t SuperStyle -> VtyWidget t m (Event t SuperStyle)
holdSuperStyleWidget ssDyn = mdo
  (focusDyn,(tl,v,bl,h,f,tr,br)) <- beginParamsLayout $ col $ do
    (tl'',v'',bl'') <- fixedD 2 $ row $ do
      tl' <- fixed 1 $ makeSuperStyleTextEntry SSC_TL ssDyn
      v' <- fixed 1 $ makeSuperStyleTextEntry SSC_V ssDyn
      bl' <- fixed 1 $ makeSuperStyleTextEntry SSC_BL ssDyn
      return (tl',v',bl')
    (h'',f'') <- fixedD 2 $ row $ do
      h' <- fixed 1 $ makeSuperStyleTextEntry SSC_H ssDyn
      f' <- fixed 1 $ makeSuperStyleTextEntry SSC_Fill ssDyn
      _ <- fixed 1 $ emptyWidget
      return (h',f')
    (tr'',br'') <- fixedD 2 $ row $ do
      tr' <- fixed 1 $ makeSuperStyleTextEntry SSC_TR ssDyn
      _ <- fixed 1 $ emptyWidget
      br' <- fixed 1 $ makeSuperStyleTextEntry SSC_BR ssDyn
      return (tr',br')
    return (tl'',v'',bl'',h'',f'',tr'',br'')
  return $ makeSuperStyleEvent tl v bl h f tr br (void $ updated focusDyn)


type ParamsSelector a = (Eq a) => SuperSEltLabel -> Maybe a

selectParamsFromSelection :: (Eq a) => Selection -> ParamsSelector a -> Maybe a
selectParamsFromSelection selection ps = r where
  params = catMaybes . toList . fmap (\ssl@(rid,_,_) -> ps ssl >>= \a -> Just (rid, a)) $ selection
  values = fmap snd params
  r = case values of
    [] -> Nothing
    x:xs -> if L.allSame values
      then Just x
      else Nothing


holdTextAlignmentWidget :: forall t m. (MonadWidget t m) => Dynamic t (Selection, Maybe TextAlign) -> VtyWidget t m (Event t ControllersWithId)
holdTextAlignmentWidget taDyn = (switchHold never =<<) . networkView . ffor taDyn $ \(selection, mta) -> do
  let
    startAlign = case mta of
      Nothing -> []
      Just TextAlign_Left -> [0]
      Just TextAlign_Center -> [1]
      Just TextAlign_Right -> [2]

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
  return (alignmentParamsEv)

emptyWidget :: (Monad m) => VtyWidget t m ()
emptyWidget = return ()

paramsLayout :: (MonadWidget t m) => [VtyWidget t m (Dynamic t Int, Event t ControllersWithId)] -> VtyWidget t m (Event t ControllersWithId)
paramsLayout widgets = fmap snd $ beginNoNavLayout $ col $ do
  outputs <- forM widgets $ \w -> mdo
    (sz, ev) <- fixed sz w
    return ev
  return $ leftmostWarn "paramsLayout" outputs

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

  -- TODO read canvasSelection and figure out what the preset is


{-
  let
    textAlignmentWidget = do

      -- tepm alignment stuff
      setAlignmentDyn <- radioListSimple 0 ["left","center","right"]
      let
        setAlignmentEv = fmap (\case
            0 -> TextAlign_Left
            1 -> TextAlign_Center
            2 -> TextAlign_Right
          ) $ updated setAlignmentDyn
        pushAlignmentFn :: TextAlign -> PushM t (Maybe ControllersWithId)
        pushAlignmentFn ta = do
          selection <- sample . current $ _goatWidget_selection (_pFWidgetCtx_goatWidget _paramsWidgetConfig_pfctx)
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
-}

  let
    selectionDyn = _goatWidget_selection (_pFWidgetCtx_goatWidget _paramsWidgetConfig_pfctx)
    textAlignSelector = (fmap (\(TextStyle ta) -> ta)) . getSEltLabelBoxTextStyle . thd3
    textAlignInputDyn = fmap (\selection -> (selection, selectParamsFromSelection selection textAlignSelector)) selectionDyn
    textAlignmentWidget :: VtyWidget t m (Dynamic t Int, Event t ControllersWithId) = fmap (1,) $ holdTextAlignmentWidget textAlignInputDyn


  let
    superStyleWidget = do
      typeChoiceDyn <- radioListSimple 0 ["presets", "custom"]

      -- this is just a potato implementation to get us started
      setStyleEvEv <- networkView $ ffor typeChoiceDyn $ \case
        0 -> do
          setStyleEv' <- beginLayout $ col $ do
            fixed 1 emptyWidget -- just to make a space
            presetClicks <- forM presetStyles $ \s -> fixedD 1 $ row $ stretch $ do
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

  outputEv <- paramsLayout [textAlignmentWidget, superStyleWidget]

  return ParamsWidget {
    _paramsWidget_paramsEvent = outputEv
  }
