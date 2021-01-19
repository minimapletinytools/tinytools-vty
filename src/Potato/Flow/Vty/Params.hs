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
import qualified Potato.Data.Text.Zipper as TZ

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Vty



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


  -- tepm alignment stuff
  setAlignmentDyn <- radioListSimple 0 ["left","center","right"]
  let
    setAlignmentEv = fmap (\case
        0 -> TextAlign_Left
        1 -> TextAlign_Center
        2 -> TextAlign_Right
      ) $ updated setAlignmentDyn

  let
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


  -- TODO insert a space here D:


  typeChoiceDyn <- radioListSimple 0 ["presets", "custom"]

  -- this is just a potato implementation to get us started
  setStyleEvEv <- networkView $ ffor typeChoiceDyn $ \case
    0 -> do
      setStyleEv' <- col $ do
        fixed 1 (return ())
        presetClicks <- forM presetStyles $ \s -> fixed 1 $ row $ stretch $ do
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

  return ParamsWidget {
    _paramsWidget_paramsEvent = leftmost [ssparamsEv, alignmentParamsEv]
  }
