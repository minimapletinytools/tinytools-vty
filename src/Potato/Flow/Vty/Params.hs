{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Flow.Vty.Params (
  ParamsWidgetConfig(..)
  , ParamsWidget(..)
  , holdParamsWidget

  -- exposed for testing
  , selectParamsFromSelection
  , networkParamsWidgetOutputDynForTesting
  , holdSuperStyleWidget
) where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Common
import           Potato.Reflex.Vty.Helpers
import Potato.Flow.Vty.PotatoReader
import Potato.Flow.Vty.Attrs
import Potato.Reflex.Vty.Widget.TextInputHelpers

import           Control.Monad.Fix
import           Control.Monad.NodeId
import           Data.Align
import           Data.Char                         (isNumber)
import           Data.Dependent.Sum                (DSum ((:=>)))
import qualified Data.IntMap                       as IM
import qualified Data.List.Extra                   as L
import qualified Data.Maybe
import qualified Data.Sequence                     as Seq
import qualified Data.Text                         as T
import qualified Data.Text.Zipper                  as TZ
import           Data.These
import           Data.Tuple.Extra
import qualified Data.List as List

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

deriving instance Show FocusId


paramsNavigation :: (MonadWidget t m) => m (Event t Int)
paramsNavigation = do
  tabEv <- key (V.KChar '\t')
  returnEv <- key V.KEnter
  let fwd  = fmap (const 1) $ leftmost [tabEv, returnEv]
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

-- TODO figure out how to do norepeat, also why does this norepeat actually not repeat for super style widget?
noRepeatNavigation :: (MonadWidget t m, HasFocus t m) => m ()
noRepeatNavigation = do
  navEv <- paramsNavigation
  requestFocus $ Refocus_Shift <$> navEv


-- Maybe Params stuff

-- | method type for picking out params from SuperSEltLabel
type ParamsSelector a = (Eq a) => SuperOwl -> Maybe a
-- | method type for picking out params when there is no selection
type DefaultParamsSelector a = PotatoDefaultParameters -> a
type ToolOverrideSelector = Tool -> Bool


toolOverrideTextAlign :: ToolOverrideSelector
toolOverrideTextAlign = (== Tool_Text)

toolOverrideSuperStyle :: ToolOverrideSelector
toolOverrideSuperStyle = (\t -> t == Tool_Box || t == Tool_Text || t == Tool_Line || t == Tool_CartLine)

toolOverrideLineStyle :: ToolOverrideSelector
toolOverrideLineStyle = (\t -> t == Tool_Line || t == Tool_CartLine)

toolOverrideSBoxType :: ToolOverrideSelector
toolOverrideSBoxType = (const False) -- NOTE default variant here does nothing as this is always overriden based on tool


-- | method to extract common parameters from a selection
-- returns Nothing if nothing in the selection has the selected param
-- returns Just (selection, Nothing) if selection that has the selected param do not share the same value
selectParamsFromSelection :: (Eq a) => ParamsSelector a -> Selection -> Maybe (Selection, Maybe a)
selectParamsFromSelection ps (SuperOwlParliament selection) = r where
  -- TODO don't do list conversion in between whataver ugh
  params = catMaybes . toList . fmap (\sowl -> ps sowl >>= \a -> Just (sowl, a)) $ selection
  values = fmap snd params
  subSelection = SuperOwlParliament $ Seq.fromList $ fmap fst params
  r = case values of
    [] -> Nothing
    x:xs -> if L.allSame values
      then Just (subSelection, Just x)
      else Just (subSelection, Nothing)

makeParamsInputDyn :: (Eq a) => ToolOverrideSelector -> ParamsSelector a -> DefaultParamsSelector a -> Tool -> Selection -> PotatoDefaultParameters -> Maybe (Selection, Maybe a, Tool)
makeParamsInputDyn tooloverridef psf dpsf tool selection pdp = r where
  nsel = isParliament_length selection
  r = if tooloverridef tool
    then Just (selection, Just (dpsf pdp), tool)
    else fmap (\(a,b) -> (a,b,tool)) $ selectParamsFromSelection psf selection

type MaybeParamsWidgetOutputDyn t m b = Dynamic t (Maybe (m (Dynamic t Int, Event t (), Event t b)))
type ParamsWidgetOutputDyn t m b = Dynamic t (m (Dynamic t Int, Event t (), Event t b))
-- if the `Maybe a` part is `Nothing` then the selection has different such properties
type ParamsWidgetFn t m a b = Dynamic t PotatoDefaultParameters -> Dynamic t (Selection, Maybe a, Tool) -> ParamsWidgetOutputDyn t m b

networkParamsWidgetOutputDynForTesting :: (MonadWidget t m, HasPotato t m) => ParamsWidgetOutputDyn t m b -> m (Dynamic t Int, Event t (), Event t b)
networkParamsWidgetOutputDynForTesting p = do
  out' <- networkView p
  outHeightDyn <- holdDyn (constDyn 0) $ fmap fst3 out'
  outCaptureEv <- switchHold never $ fmap snd3 out'
  outEv <- switchHold never $ fmap thd3 out'
  return (join outHeightDyn, outCaptureEv, outEv)


-- |
-- returned Dynamic contains Nothing if selection was Nothing, otherwise contains Just the widget to modify parameters
-- remember that input dynamic must not be disconnected from output event or there will be an infinite loop!
-- maybe use delayEvent :: forall t m a. (Adjustable t m) => Event t a -> m) (Event t a) 😱
holdMaybeParamsWidget :: forall t m a b. (MonadWidget t m)
  => Dynamic t PotatoDefaultParameters
  -> Dynamic t (Maybe (Selection, Maybe a, Tool)) -- ^ selection/params input
  -> ParamsWidgetFn t m a b -- ^ function creating widget, note that it should always return non-nothing but using Maybe type makes life easier
  -> m (MaybeParamsWidgetOutputDyn t m b)
holdMaybeParamsWidget pdpDyn mInputDyn widgetFn = do
  -- only remake the widget if it goes from Just to Nothing
  uniqDyn <- holdUniqDynBy (\a b -> isJust a == isJust b) mInputDyn
  return . join . ffor uniqDyn $ \case
    Nothing -> constDyn Nothing
    -- eh this is weird, fromMaybe should always succeed, maybe using fromJust is ok due to laziness but I don't care to find out
    Just _ -> Just <$> widgetFn pdpDyn (fmap (fromMaybe (isParliament_empty, Nothing, Tool_Select)) mInputDyn)

emptyWidget :: (Monad m) => m ()
emptyWidget = return ()



-- SuperStyle stuff
data SuperStyleCell = SSC_TL | SSC_TR | SSC_BL | SSC_BR | SSC_V | SSC_H | SSC_Fill deriving (Show)

updateFromSuperStyle :: SuperStyleCell -> (SuperStyle -> TZ.TextZipper)
updateFromSuperStyle ssc = TZ.top . TZ.fromText . T.singleton . gettfn ssc where
  gettfn ssc' = fromMaybe ' ' . gettfn' ssc'
  gettfn' = \case
    SSC_TL -> _superStyle_tl
    SSC_TR -> _superStyle_tr
    SSC_BL -> _superStyle_bl
    SSC_BR -> _superStyle_br
    SSC_V -> _superStyle_vertical
    SSC_H -> _superStyle_horizontal
    SSC_Fill -> (\case
      FillStyle_Simple c -> Just c
      _ -> Nothing) . _superStyle_fill


makeSuperStyleTextEntry :: (MonadWidget t m, HasPotato t m) => SuperStyleCell -> Dynamic t (Maybe SuperStyle) -> m (Behavior t PChar)
makeSuperStyleTextEntry ssc mssDyn = do
  mss0 <- sample . current $ mssDyn
  let modifyEv = (fmap (maybe id (\ss -> const (updateFromSuperStyle ssc ss))) (updated mssDyn))
  ti <- singleCellTextInput modifyEv $ case mss0 of
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
        -- TODO Nothing is text cell was blank...
        _superStyle_tl    = Just tl'
        , _superStyle_tr     = Just tr'
        , _superStyle_bl        = Just bl'
        , _superStyle_br         = Just br'
        , _superStyle_vertical   = Just v'
        , _superStyle_horizontal = Just h'
        --, _superStyle_point      :: PChar
        , _superStyle_fill       = FillStyle_Simple f'
      }

presetSuperStyles :: [[Char]]
presetSuperStyles = ["╔╗╚╝║═ ","****|- ", "██████ ", "┌┐└┘│─ "]

holdSuperStyleWidget :: forall t m. (MonadLayoutWidget t m, HasPotato t m) => ParamsWidgetFn t m SuperStyle (Either ControllersWithId SetPotatoDefaultParameters)
holdSuperStyleWidget pdpDyn inputDyn = constDyn $ mdo

  typeChoiceDyn <- radioListSimple 0 ["custom", "presets"]

  setStyleEvEv <- networkView $ ffor typeChoiceDyn $ \case
    1 -> do
      setStyleEv' <- initLayout $ col $ do
        (grout . fixed) 1 emptyWidget -- just to make a space
        presetClicks <- forM presetSuperStyles $ \s -> (grout . fixed) 1 $ row $ (grout . stretch) 1 $ do
          -- TODO highlight if style matches selection
          text (constant (T.pack s))
          fmap (fmap (\_ -> s)) (mouseDown V.BLeft)
        return $ fmap superStyle_fromListFormat (leftmost presetClicks)
      return (5, never, setStyleEv')
    0 -> do
      -- TODO the awesome version of this has a toggle box so that you can choose to do horiz/vertical together (once you support separate horiz/vert left/right/top/down styles)
      -- TODO also a toggle for setting corners to common sets
      let
        mssDyn = fmap snd3 inputDyn
      -- TODO arrow nav would be super cool
      noRepeatNavigation
      (focusDyn,tl,v,bl,h,f,tr,br) <- col $ do
        (grout . fixed) 1 emptyWidget -- just to make a space
        --(tile . fixed) 1 $ text (fmap (T.pack . superStyle_toListFormat . Data.Maybe.fromJust) $ current mssDyn)
        (tl'',h'',tr'') <- (grout . fixed) 1 $ row $ do
          tl' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_TL mssDyn
          h' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_H mssDyn
          tr' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_TR mssDyn
          return (tl',h',tr')
        (v'',f'') <- (grout . fixed) 1 $ row $ do
          v' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_V mssDyn
          f' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_Fill mssDyn
          _ <- (grout . fixed) 1 $ emptyWidget -- TODO you can modify this too, why not, 2 boxes for the same thing
          return (v',f')
        (bl'',br'') <- (grout . fixed) 1 $ row $ do
          bl' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_BL mssDyn
          _ <- (grout . fixed) 1 $ emptyWidget -- TODO you can modify this too, why not, 2 boxes for the same thing
          br' <- (tile . fixed) 1 $ makeSuperStyleTextEntry SSC_BR mssDyn
          return (bl',br')
        focusDyn' <- focusedId
        return (focusDyn',tl'',v'',bl'',h'',f'',tr'',br'')
      captureEv1 <- makeCaptureFromUpdateTextZipperMethod updateTextZipperForSingleCharacter

      focusDynUnique <- holdUniqDyn focusDyn

      let
        -- TODO maybe just do it when any of the cell dynamics are updated rather than when focus changes...
        -- TODO if we do it on focus change, you don't want to set when escape is pressed... so maybe it's better just to do 🖕
        setStyleEv' = makeSuperStyleEvent tl v bl h f tr br (void $ updated focusDynUnique)
        captureEv' = leftmost [void setStyleEv', captureEv1]
      return (4, captureEv', setStyleEv')



  setStyleEv <- switchHold never (fmap thd3 setStyleEvEv)
  captureEv <- switchHold never (fmap snd3 setStyleEvEv)
  heightDyn <- holdDyn 0 (fmap fst3 setStyleEvEv)

  let
    selectionDyn = fmap fst3 inputDyn
    pushSuperStyleFn :: SuperStyle -> PushM t (Maybe (Either ControllersWithId SetPotatoDefaultParameters))
    pushSuperStyleFn ss = do
      (SuperOwlParliament selection, _, tool) <- sample . current $ inputDyn
      pdp <- sample . current $ pdpDyn
      let
        fmapfn sowl = case getSEltLabelSuperStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldss -> if oldss == ss
            then Nothing
            else Just (_superOwl_id sowl, CTagSuperStyle :=> Identity (CSuperStyle (DeltaSuperStyle (oldss, ss))))
      return $ if toolOverrideSuperStyle tool
        then if _potatoDefaultParameters_superStyle pdp == ss
          then Nothing
          else Just . Right $ def { _setPotatoDefaultParameters_superStyle = Just ss }
        else case Data.Maybe.mapMaybe fmapfn . toList $ selection of
          [] -> Nothing
          x  -> Just . Left $ IM.fromList x
    ssparamsEv = push pushSuperStyleFn setStyleEv
  return (ffor heightDyn (+1), captureEv, ssparamsEv)

data LineStyleCell = LSC_L | LSC_R | LSC_U | LSC_D

updateFromLineStyle :: LineStyleCell -> (LineStyle -> TZ.TextZipper)
updateFromLineStyle lsc = TZ.top . TZ.fromText . gettfn lsc where
  gettfn = \case
    LSC_L -> _lineStyle_leftArrows
    LSC_R -> _lineStyle_rightArrows
    LSC_U -> _lineStyle_upArrows
    LSC_D -> _lineStyle_downArrows

makeLineStyleEvent :: (Reflex t)
  => Behavior t Text
  -> Behavior t Text
  -> Behavior t Text
  -> Behavior t Text
  -> Event t ()
  -> Event t LineStyle
makeLineStyleEvent l r u d trig = pushAlways pushfn trig where
  pushfn _ = do
    l' <- sample l
    r' <- sample r
    u' <- sample u
    d' <- sample d
    return $ def {
        _lineStyle_leftArrows    = l'
        , _lineStyle_rightArrows = r'
        , _lineStyle_upArrows    = u'
        , _lineStyle_downArrows  = d'
      }

-- TODO someday do backwards expanding text entry boxes for LSC_R and LSC_D
makeLineStyleTextEntry :: (MonadWidget t m, HasPotato t m) => LineStyleCell -> Dynamic t (Maybe LineStyle) -> m (Behavior t Text)
makeLineStyleTextEntry lsc mlsDyn = do
  mls0 <- sample . current $ mlsDyn
  let modifyEv = (fmap (maybe id (\ss -> const (updateFromLineStyle lsc ss))) (updated mlsDyn))
  -- TODO need to use different text input type
  ti <- singleCellTextInput modifyEv $ case mls0 of
    Nothing  -> ""
    Just ls0 -> updateFromLineStyle lsc ls0
  return . current $ ti


presetLineStyles :: [([Char], [Char], [Char], [Char])]
presetLineStyles = [("<",">","v","^"), ("⇦","⇨","⇧","⇩")]

presetLineStyle_toText :: ([Char], [Char], [Char], [Char]) -> Text
presetLineStyle_toText (l,r,u,d) = T.pack $ l <> " " <> r <> " " <> u <> " " <> d

-- TODO lineystel widget should be like this
-- [x] start | [x] end    (the one being modified is highlighted)
-- custom | preset
-- ....
-- | ignore _lineStyle_autoStyle part of LineStyle output
holdLineStyleWidgetNew :: forall t m. (MonadLayoutWidget t m, HasPotato t m) => ParamsWidgetFn t m LineStyle (Either ControllersWithId SetPotatoDefaultParameters)
holdLineStyleWidgetNew pdpDyn inputDyn = constDyn $ do

  -- TODO in the future, we'd like to be able to disable line ends more easily (without going into presets)
  -- i.e. [x] start | [x] end
  endChoiceDyn <- radioListSimple 0 ["start", "end"]

  typeChoiceDyn <- radioListSimple 0 ["custom", "presets"]

  setStyleEvEv <- networkView $ ffor typeChoiceDyn $ \case
    1 -> do
      setStyleEv' <- initLayout $ col $ do
        (grout . fixed) 1 emptyWidget -- just to make a space
        presetClicks <- forM presetLineStyles $ \s -> (grout . fixed) 1 $ row $ (grout . stretch) 1 $ do
          -- TODO highlight if style matches selection
          text (constant (presetLineStyle_toText s))
          fmap (fmap (\_ -> s)) (mouseDown V.BLeft)
        return $ fmap lineStyle_fromListFormat (leftmost presetClicks)
      return (5, never, setStyleEv')
    0 -> do
      let
        lssDyn = fmap snd3 inputDyn

      noRepeatNavigation
      (focusDyn,l,r,u,d) <-  col $ do
        (grout . fixed) 1 emptyWidget -- just to make a space
        --(tile . fixed) 1 $ text (fmap (T.pack . superStyle_toListFormat . Data.Maybe.fromJust) $ current mssDyn)
        l_d1 <- (grout . fixed) 1 $ row $ do
          (grout . fixed) 8 $ text " left:"
          (tile . stretch) 1 $ makeLineStyleTextEntry LSC_L lssDyn
        r_d1 <- (grout . fixed) 1 $ row $ do
          (grout . fixed) 8 $ text "right:"
          (tile . stretch) 1 $ makeLineStyleTextEntry LSC_R lssDyn
        (u_d1, d_d1) <- (grout . fixed) 3 $ row $ (grout . stretch) 1 $ do
          col $ (grout . fixed) 3 $ text "up:"
          u_d2 <- col $ (tile . fixed) 1 $ makeLineStyleTextEntry LSC_U lssDyn
          col $ (grout . fixed) 5 $ text "down:"
          d_d2 <- col $ (tile . fixed) 1 $ makeLineStyleTextEntry LSC_D lssDyn
          -- pad the end
          (tile . stretch) 0 $ return ()
          return (u_d2, d_d2)
        -- pad the end
        (tile . stretch) 0 $ return ()
        focusDyn' <- focusedId
        return (focusDyn',l_d1,r_d1,u_d1,d_d1)

      captureEv'' <- makeCaptureFromUpdateTextZipperMethod updateTextZipperForSingleCharacter
      focusDynUnique <- holdUniqDyn focusDyn

      -- TODO needs more stuff here

      let
        -- TODO maybe just do it when any of the cell dynamics are updated rather than when focus changes...
        -- TODO if we do it on focus change, you don't want to set when escape is pressed... so maybe it's better just to do 🖕
        setStyleEv' = makeLineStyleEvent l r u d (void $ updated focusDynUnique)
        captureEv' = leftmost [void setStyleEv', captureEv'']
      return (6, captureEv', setStyleEv')

  setStyleEv <- switchHold never (fmap thd3 setStyleEvEv)
  captureEv <- switchHold never (fmap snd3 setStyleEvEv)
  heightDyn <- holdDyn 0 (fmap fst3 setStyleEvEv)

  let
    selectionDyn = fmap fst3 inputDyn
    pushLineStyleFn :: LineStyle -> PushM t (Maybe (Either ControllersWithId SetPotatoDefaultParameters))
    pushLineStyleFn ss = do
      pdp <- sample . current $ pdpDyn
      (SuperOwlParliament selection, _, tool) <- sample . current $ inputDyn
      let
        overrideAutoStyle oldss newss = newss { _lineStyle_autoStyle = _lineStyle_autoStyle oldss }
        fmapfn sowl = case getSEltLabelLineStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldss -> if oldss == overrideAutoStyle oldss ss
            then Nothing
            else Just (_superOwl_id sowl, CTagLineStyle :=> Identity (CLineStyle (DeltaLineStyle (oldss, overrideAutoStyle oldss ss))))
      return $ if toolOverrideLineStyle tool
        then if _potatoDefaultParameters_lineStyle pdp == ss
          then Nothing
          else Just . Right $ def { _setPotatoDefaultParameters_lineStyle = Just ss }
        else case Data.Maybe.mapMaybe fmapfn . toList $ selection of
          [] -> Nothing
          x  -> Just . Left $ IM.fromList x
    ssparamsEv = push pushLineStyleFn setStyleEv

  return (constDyn 6, captureEv, ssparamsEv)

-- TODO DELETE replace with holdLineStyleWidgetNew (when it's ready)
-- | ignore _lineStyle_autoStyle part of LineStyle output
holdLineStyleWidget :: forall t m. (MonadLayoutWidget t m, HasPotato t m) => ParamsWidgetFn t m LineStyle (Either ControllersWithId SetPotatoDefaultParameters)
holdLineStyleWidget pdpDyn inputDyn = constDyn $ do

  let
    lssDyn = fmap snd3 inputDyn

  noRepeatNavigation
  (focusDyn,l,r,u,d) <-  col $ do
    (grout . fixed) 1 emptyWidget -- just to make a space
    --(tile . fixed) 1 $ text (fmap (T.pack . superStyle_toListFormat . Data.Maybe.fromJust) $ current mssDyn)
    l_d1 <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text " left:"
      (tile . stretch) 1 $ makeLineStyleTextEntry LSC_L lssDyn
    r_d1 <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text "right:"
      (tile . stretch) 1 $ makeLineStyleTextEntry LSC_R lssDyn
    (u_d1, d_d1) <- (grout . fixed) 3 $ row $ (grout . stretch) 1 $ do
      col $ (grout . fixed) 3 $ text "up:"
      u_d2 <- col $ (tile . fixed) 1 $ makeLineStyleTextEntry LSC_U lssDyn
      col $ (grout . fixed) 5 $ text "down:"
      d_d2 <- col $ (tile . fixed) 1 $ makeLineStyleTextEntry LSC_D lssDyn
      -- pad the end
      (tile . stretch) 0 $ return ()
      return (u_d2, d_d2)
    -- pad the end
    (tile . stretch) 0 $ return ()
    focusDyn' <- focusedId
    return (focusDyn',l_d1,r_d1,u_d1,d_d1)

  captureEv <- makeCaptureFromUpdateTextZipperMethod updateTextZipperForSingleCharacter
  focusDynUnique <- holdUniqDyn focusDyn

  let
    selectionDyn = fmap fst3 inputDyn
    pushLineStyleFn :: LineStyle -> PushM t (Maybe (Either ControllersWithId SetPotatoDefaultParameters))
    pushLineStyleFn ss = do
      pdp <- sample . current $ pdpDyn
      (SuperOwlParliament selection, _, tool) <- sample . current $ inputDyn
      let
        overrideAutoStyle oldss newss = newss { _lineStyle_autoStyle = _lineStyle_autoStyle oldss }
        fmapfn sowl = case getSEltLabelLineStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldss -> if oldss == overrideAutoStyle oldss ss
            then Nothing
            else Just (_superOwl_id sowl, CTagLineStyle :=> Identity (CLineStyle (DeltaLineStyle (oldss, overrideAutoStyle oldss ss))))
      return $ if toolOverrideLineStyle tool
        then if _potatoDefaultParameters_lineStyle pdp == ss
          then Nothing
          else Just . Right $ def { _setPotatoDefaultParameters_lineStyle = Just ss }
        else case Data.Maybe.mapMaybe fmapfn . toList $ selection of
          [] -> Nothing
          x  -> Just . Left $ IM.fromList x
    setStyleEv = makeLineStyleEvent l r u d (void $ updated focusDynUnique)
    ssparamsEv = push pushLineStyleFn setStyleEv


  return (constDyn 6, captureEv, ssparamsEv)



-- Text Alignment stuff
holdTextAlignmentWidget :: forall t m. (MonadWidget t m) => ParamsWidgetFn t m TextAlign (Either ControllersWithId SetPotatoDefaultParameters)
holdTextAlignmentWidget _ inputDyn = constDyn $ do
  let
    mtaDyn = fmap snd3 inputDyn
    selectionDyn = fmap fst3 inputDyn

  let

    alignDyn = ffor mtaDyn $ \case
      Nothing               -> []
      Just TextAlign_Left   -> [0]
      Just TextAlign_Center -> [1]
      Just TextAlign_Right  -> [2]

  -- I'm actually not sure why using alignDyn here isn't causing an infinite loop
  -- I guess the whole widget is getting recreated when alignment changes... but when I sampled alignDyn instead, it didn't update correctly 🤷🏼‍♀️
  (setAlignmentEv', _) <- radioList (constDyn ["left","center","right"]) alignDyn Nothing

  let
    setAlignmentEv = fmap (\case
        0 -> TextAlign_Left
        1 -> TextAlign_Center
        2 -> TextAlign_Right
      ) $ setAlignmentEv'
    pushAlignmentFn :: TextAlign -> PushM t (Maybe (Either ControllersWithId SetPotatoDefaultParameters))
    pushAlignmentFn ta = do
      (SuperOwlParliament selection, _, tool) <- sample . current $ inputDyn
      let
        fmapfn sowl = case getSEltLabelBoxTextStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldts -> if oldts == TextStyle ta
            then Nothing
            else Just (_superOwl_id sowl, CTagBoxTextStyle :=> Identity (CTextStyle (DeltaTextStyle (oldts, TextStyle ta))))
      return $ if toolOverrideTextAlign tool
        then Just . Right $ def { _setPotatoDefaultParameters_box_text_textAlign = Just ta }
        else case Data.Maybe.mapMaybe fmapfn . toList $ selection of
          [] -> Nothing
          x  -> Just . Left $ IM.fromList x
    alignmentParamsEv = push pushAlignmentFn setAlignmentEv

  return (1, never, alignmentParamsEv)

holdSBoxTypeWidget :: forall t m. (MonadLayoutWidget t m) => ParamsWidgetFn t m SBoxType (Either ControllersWithId SetPotatoDefaultParameters)
holdSBoxTypeWidget _ inputDyn = constDyn $ do
  let
    mBoxType = fmap snd3 inputDyn
    selectionDyn = fmap fst3 inputDyn
  mbt0 <- sample . current $ mBoxType

  let
    stateDyn = ffor mBoxType $ \case
      -- Not great, this will override everything in selection without having a "grayed out state" and do the override in a not so great way, but whatever
      Nothing                 -> (False,False)
      Just SBoxType_Box       -> (True,False)
      Just SBoxType_BoxText   -> (True,True)
      Just SBoxType_NoBox     -> (False,False)
      Just SBoxType_NoBoxText -> (False,True)

    borderDyn = fmap fst stateDyn
    textDyn = fmap snd stateDyn

  (b,t) <- col $ do
    b_d1 <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text "border:"
      (grout . stretch) 1 $ checkBox borderDyn
    t_d1 <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text "  text:"
      (grout . stretch) 1 $ checkBox textDyn
    return (b_d1,t_d1)

  let
    captureEv = void $ leftmost [b,t]

    pushSBoxTypeFn :: These Bool Bool -> PushM t (Maybe (Either ControllersWithId SetPotatoDefaultParameters))
    pushSBoxTypeFn bt = do
      (SuperOwlParliament selection, _, tool) <- sample . current $ inputDyn
      curState <- sample . current $ stateDyn
      let
        fmapfn sowl = case getSEltLabelBoxType (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldbt -> if oldbt == newbt
            then Nothing
            else Just (_superOwl_id sowl, CTagBoxType :=> Identity (CBoxType (oldbt, newbt)))
            where
              newbt = case bt of
                This border -> make_sBoxType border (sBoxType_isText oldbt)
                That text -> make_sBoxType (sBoxType_hasBorder oldbt) text
                These border text -> make_sBoxType border text
      return $  if toolOverrideSBoxType tool
        -- UNTESTED, it's probably currect but the tool overrides this default so I never actually tested it
        then Just . Right $ def { _setPotatoDefaultParameters_sBoxType = Just $ case bt of
            This border -> make_sBoxType border (snd curState)
            That text -> make_sBoxType (fst curState) text
            These border text -> make_sBoxType border text
          }
        else case Data.Maybe.mapMaybe fmapfn . toList $ selection of
          [] -> Nothing
          x  -> Just . Left $ IM.fromList x
    sBoxTypeParamsEv = push pushSBoxTypeFn (align b t)

  -- TODO
  return (2, captureEv, sBoxTypeParamsEv)

holdCanvasSizeWidget :: forall t m. (MonadLayoutWidget t m, HasPotato t m) => Dynamic t SCanvas -> ParamsWidgetFn t m () XY
holdCanvasSizeWidget canvasDyn _ nothingDyn = ffor nothingDyn $ \_ -> do
  let
    cSizeDyn = fmap (_lBox_size . _sCanvas_box) canvasDyn
    cWidthDyn = fmap (\(V2 x _) -> x) cSizeDyn
    cHeightDyn = fmap (\(V2 _ y) -> y) cSizeDyn
  noRepeatNavigation
  (focusDyn,wDyn,hDyn) <- col $ do
    wDyn' <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text " width:"
      (tile . stretch) 1 $ dimensionInput cWidthDyn
    hDyn' <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text "height:"
      (tile . stretch) 1 $ dimensionInput cHeightDyn
    focusDyn' <- focusedId
    return (focusDyn',wDyn',hDyn')
  focusDynUnique <- holdUniqDyn focusDyn
  let
    outputEv = flip push (void $ updated focusDynUnique) $ \_ -> do
      cw <- sample . current $ cWidthDyn
      ch <- sample . current $ cHeightDyn
      w <- sample . current $ wDyn
      h <- sample . current $ hDyn
      return $ if cw /= w || ch /= h
        then Just $ V2 (w-cw) (h-ch) -- it's a delta D:
        else Nothing
  captureEv1 <- makeCaptureFromUpdateTextZipperMethod updateTextZipperForNumberInput
  let
    -- causes causality loop idk why :(
    --captureEv = leftmost [void outputEv, void (updated wDyn), void (updated hDyn)]
    captureEv = leftmost [void outputEv, captureEv1]
  return (2, captureEv, outputEv)

data SEltParams = SEltParams {
    --_sEltParams_sBox =
  }

data ParamsWidgetConfig t = ParamsWidgetConfig {
   _paramsWidgetConfig_selectionDyn :: Dynamic t Selection
  , _paramsWidgetConfig_canvasDyn :: Dynamic t SCanvas
  , _paramsWidgetConfig_defaultParamsDyn :: Dynamic t PotatoDefaultParameters
  , _paramsWidgetConfig_toolDyn :: Dynamic t Tool
  -- many params don't set anything until they lose focus. However if we lose focus because we clicked onto another pane, that focus event doesn't propogate down far enough so we have to pass it down manually
  , _paramsWidgetConfig_loseFocusEv :: Event t ()
}

data ParamsWidget t = ParamsWidget {
  _paramsWidget_paramsEvent       :: Event t ControllersWithId
  , _paramsWidget_canvasSizeEvent :: Event t XY
  , _paramsWidget_setDefaultParamsEvent :: Event t SetPotatoDefaultParameters
  , _paramsWidget_captureInputEv  :: Event t ()

  , _paramsWidget_widgetHeight :: Dynamic t Int

}

-- TODO move to potato reflex
switchHoldPair :: (Reflex t, MonadHold t m) => Event t a -> Event t b -> Event t (Event t a, Event t b) -> m (Event t a, Event t b)
switchHoldPair eva evb evin = fmap fanThese $ switchHold (align eva evb) $ fmap (uncurry align) evin

-- TODO move to potato reflex
switchHoldTriple :: forall t m a b c. (Reflex t, MonadHold t m) => Event t a -> Event t b -> Event t c -> Event t (Event t a, Event t b, Event t c) -> m (Event t a, Event t b, Event t c)
switchHoldTriple eva evb evc evin = r where
  evinAligned :: Event t (Event t (These a (These b c)))
  evinAligned = fmap (\(eva', evb', evc') -> align eva' (align evb' evc')) evin
  evabc = align eva (align evb evc)
  switched :: m (Event t (These a (These b c)))
  switched = switchHold evabc evinAligned
  fanned1 :: m (Event t a, Event t (These b c))
  fanned1 = fmap fanThese switched
  fanned2 = fmap (\(a,bc) -> (a, fanThese bc)) fanned1
  r = fmap (\(a, (b,c)) -> (a,b,c)) fanned2

joinHold :: (Reflex t, MonadHold t m) => Event t (Dynamic t a) -> Dynamic t a -> m (Dynamic t a)
joinHold ev d0 = do
  dyndyn <- holdDyn d0 ev
  return $ join dyndyn

fth4 :: (a,b,c,d) -> d
fth4 (_,_,_,d) = d

fstsndthd4 :: (a,b,c,d) -> (a,b,c)
fstsndthd4 (a,b,c,_) = (a,b,c)

holdParamsWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => ParamsWidgetConfig t
  -> m (ParamsWidget t)
holdParamsWidget ParamsWidgetConfig {..} = do
  let
    selectionDyn = _paramsWidgetConfig_selectionDyn
    canvasDyn = _paramsWidgetConfig_canvasDyn
    defaultParamsDyn = _paramsWidgetConfig_defaultParamsDyn
    toolDyn = _paramsWidgetConfig_toolDyn

    mTextAlignInputDyn = ffor3 toolDyn selectionDyn defaultParamsDyn $ makeParamsInputDyn
      toolOverrideTextAlign
      ((fmap (\(TextStyle ta) -> ta)) . getSEltLabelBoxTextStyle . superOwl_toSEltLabel_hack)
      _potatoDefaultParameters_box_text_textAlign
    mSuperStyleInputDyn = ffor3 toolDyn selectionDyn defaultParamsDyn $ makeParamsInputDyn
      toolOverrideSuperStyle
      (getSEltLabelSuperStyle . superOwl_toSEltLabel_hack)
      _potatoDefaultParameters_superStyle
    mLineStyleInputDyn = ffor3 toolDyn selectionDyn defaultParamsDyn $ makeParamsInputDyn
      toolOverrideLineStyle
      (getSEltLabelLineStyle . superOwl_toSEltLabel_hack)
      _potatoDefaultParameters_lineStyle
    mSBoxTypeInputDyn = ffor3 toolDyn selectionDyn defaultParamsDyn $ makeParamsInputDyn
      toolOverrideSBoxType
      (getSEltLabelBoxType . superOwl_toSEltLabel_hack)
      _potatoDefaultParameters_sBoxType

    -- show canvas params when nothing is selected
    mCanvasSizeInputDyn = ffor2 toolDyn selectionDyn (\t s -> if isParliament_null s then Just (isParliament_empty, Nothing, t) else Nothing)

  (paramsOutputEv, captureEv, canvasSizeOutputEv, heightDyn) <- initManager_ $ do
    requestFocus $ (Refocus_Clear <$ _paramsWidgetConfig_loseFocusEv)
    textAlignmentWidget <- holdMaybeParamsWidget defaultParamsDyn mTextAlignInputDyn holdTextAlignmentWidget
    superStyleWidget2 <- holdMaybeParamsWidget defaultParamsDyn mSuperStyleInputDyn holdSuperStyleWidget
    --lineStyleWidget <- holdMaybeParamsWidget defaultParamsDyn mLineStyleInputDyn holdLineStyleWidget
    lineStyleWidget <- holdMaybeParamsWidget defaultParamsDyn mLineStyleInputDyn holdLineStyleWidgetNew
    sBoxTypeWidget <- holdMaybeParamsWidget defaultParamsDyn mSBoxTypeInputDyn holdSBoxTypeWidget
    canvasSizeWidget <- holdMaybeParamsWidget defaultParamsDyn mCanvasSizeInputDyn (holdCanvasSizeWidget canvasDyn)

    -- apparently textAlignmentWidget gets updated after any change which causes the whole network to rerender and we lose our focus state...
    let
      controllersWithIdParamsWidgets = fmap catMaybes . mconcat . (fmap (fmap (:[]))) $ [textAlignmentWidget, superStyleWidget2, lineStyleWidget, sBoxTypeWidget]

    paramsNetwork <- networkView . ffor2 controllersWithIdParamsWidgets canvasSizeWidget $ \widgets mcsw -> col $ do
      outputs <- forM widgets $ \w -> mdo
        (sz, captureEv', ev) <- (tile . fixed) sz w
        return (sz, ev, captureEv')
      -- canvas size widget is special becaues it's output type is different
      (cssz, cssev, captureEv2) <- case mcsw of
        Nothing -> return (0, never, never)
        Just csw -> mdo
          (cssz', csCaptureEv', cssev') <- (tile . fixed) cssz' csw
          return (cssz', cssev', csCaptureEv')
      let
        heightDyn'' = liftA2 (+) cssz $ foldr (liftA2 (+)) 0 $ fmap fst3 outputs
      return $ (leftmostWarn "paramsLayout" (fmap snd3 outputs), leftmostWarn "paramsCapture" (captureEv2 : fmap thd3 outputs), cssev, heightDyn'')

    heightDyn' <- joinHold (fmap fth4 paramsNetwork) 0
    (paramsOutputEv', captureEv', canvasSizeOutputEv') <- switchHoldTriple never never never $ fmap fstsndthd4 paramsNetwork

    return (paramsOutputEv', captureEv', canvasSizeOutputEv', heightDyn')

  let
    maybeLeft (Left a) = Just a
    maybeLeft _ = Nothing
    maybeRight (Right a) = Just a
    maybeRight _ = Nothing

  return ParamsWidget {
    _paramsWidget_paramsEvent = fmapMaybe maybeLeft paramsOutputEv
    , _paramsWidget_canvasSizeEvent = canvasSizeOutputEv
    , _paramsWidget_setDefaultParamsEvent = fmapMaybe maybeRight paramsOutputEv
    , _paramsWidget_captureInputEv = captureEv
    , _paramsWidget_widgetHeight = heightDyn
  }
