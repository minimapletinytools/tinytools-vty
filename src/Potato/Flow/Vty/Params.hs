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

import qualified Graphics.Vty                      as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty

deriving instance Show FocusId

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

-- | capture matches updateTextZipperForSingleCharacter
singleCharacterCapture :: (Reflex t, MonadFix m, MonadNodeId m, HasInput t m) => m (Event t ())
singleCharacterCapture = do
  inp <- input
  return $ fforMaybe inp $ \case
    V.EvKey (V.KChar '\t') [] -> Nothing
    V.EvKey (V.KChar k) [] -> Just ()
    V.EvKey V.KBS [] -> Just ()
    V.EvKey V.KDel [] -> Just ()
    V.EvKey (V.KChar 'u') [V.MCtrl] -> Just ()
    _ -> Nothing

-- | Default vty event handler for text inputs
updateTextZipperForNumberInput
  :: V.Event -- ^ The vty event to handle
  -> TZ.TextZipper -- ^ The zipper to modify
  -> TZ.TextZipper
updateTextZipperForNumberInput ev = case ev of
  V.EvKey (V.KChar k) [] | isNumber k -> TZ.insertChar k
  V.EvKey V.KBS []                    -> TZ.deleteLeft
  V.EvKey V.KDel []                   -> TZ.deleteRight
  V.EvKey V.KLeft []                  -> TZ.left
  V.EvKey V.KRight []                 -> TZ.right
  V.EvKey V.KHome []                  -> TZ.home
  V.EvKey V.KEnd []                   -> TZ.end
  V.EvKey (V.KChar 'u') [V.MCtrl]     -> const TZ.empty
  _                                   -> id

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



type MaybeParamsWidgetOutputDyn t m b = Dynamic t (Maybe (m (Dynamic t Int, Event t (), Event t b)))

type ParamsWidgetOutputDyn t m b = Dynamic t (m (Dynamic t Int, Event t (), Event t b))
type ParamsWidgetFn t m a b = Dynamic t (Selection, Maybe a) -> ParamsWidgetOutputDyn t m b

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
  => Dynamic t (Maybe (Selection, Maybe a)) -- ^ selection/params input
  -> ParamsWidgetFn t m a b -- ^ function creating widget, note that it should always return non-nothing but using Maybe type makes life easier
  -> m (MaybeParamsWidgetOutputDyn t m b)
holdMaybeParamsWidget mInputDyn widgetFn = do
  -- only remake the widget if it goes from Just to Nothing
  uniqDyn <- holdUniqDynBy (\a b -> isJust a == isJust b) mInputDyn
  return . join . ffor uniqDyn $ \case
    Nothing -> constDyn Nothing
    -- eh this is weird, maybe using fromJust is ok due to laziness but I don't care to find out
    Just _ -> Just <$> widgetFn (fmap (fromMaybe (isParliament_empty, Nothing)) mInputDyn)

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

singleCellTextInput
  :: (MonadWidget t m, HasPotato t m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> m (Dynamic t Text)
singleCellTextInput modifyEv c0 = do
  i <- input
  textInputCustom (mergeWith (.) [fmap updateTextZipperForSingleCharacter i, modifyEv]) c0


-- remember that input dyn can't update the same time the output updates or you will have infinite loop
dimensionInput
  :: (MonadWidget t m, HasPotato t m)
  => Dynamic t Int
  -> m (Dynamic t Int)
dimensionInput valueDyn = do



  let
    toText = TZ.fromText . show
    modifyEv = fmap (\v -> const (toText v)) (updated valueDyn)
  v0 <- sample . current $ valueDyn
  i <- input
  tDyn <- textInputCustom (mergeWith (.) [fmap updateTextZipperForNumberInput i, modifyEv]) (toText v0)
  --tDyn <- fmap _textInput_value $ textInput (def { _textInputConfig_initialValue = (toText v0)})
  return $ ffor2 valueDyn tDyn $ \v t -> fromMaybe v (readMaybe (T.unpack t))

-- TODO use theming here
textInputCustom
  :: (MonadWidget t m, HasPotato t m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> m (Dynamic t Text)
textInputCustom modifyEv c0 = mdo
  f <- focus
  dh <- displayHeight
  dw <- displayWidth

  -- TODO do this without sampling (I think this will not update if you change style without recreating these widgets)
  -- (you could do this easily by using localTheme)
  potatostyle <- askPotato >>=  sample . _potatoConfig_style

  let
    cursorAttributes = _potatoStyle_selected potatostyle
    normalAttributes = _potatoStyle_normal potatostyle

  rec v <- foldDyn ($) c0 $ mergeWith (.)
        [ modifyEv
        , let displayInfo = current rows
          in ffor (attach displayInfo click) $ \(dl, MouseDown _ (mx, my) _) ->
            TZ.goToDisplayLinePosition mx my dl
        ]
      click <- mouseDown V.BLeft
      let
        cursorAttrs = ffor f $ \x -> if x then cursorAttributes else normalAttributes
      let rows = (\w s c -> TZ.displayLines w normalAttributes c s)
            <$> dw
            <*> (TZ.mapZipper <$> (constDyn id) <*> v)
            <*> cursorAttrs
          img = images . TZ._displayLines_spans <$> rows
      tellImages $ (\imgs -> (:[]) . V.vertCat $ imgs) <$> current img
  return $ TZ.value <$> v


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

holdSuperStyleWidget :: forall t m. (MonadLayoutWidget t m, HasPotato t m) => ParamsWidgetFn t m SuperStyle ControllersWithId
holdSuperStyleWidget inputDyn = constDyn $ mdo

  typeChoiceDyn <- radioListSimple 0 ["custom", "presets"]

  setStyleEvEv <- networkView $ ffor typeChoiceDyn $ \case
    1 -> do
      setStyleEv' <- initLayout $ col $ do
        (grout . fixed) 1 emptyWidget -- just to make a space
        presetClicks <- forM presetStyles $ \s -> (grout . fixed) 1 $ row $ (grout . stretch) 1 $ do
          -- TODO highlight if style matches selection
          text (constant (T.pack s))
          fmap (fmap (\_ -> s)) (mouseDown V.BLeft)
        return $ fmap superStyle_fromListFormat (leftmost presetClicks)
      return (5, never, setStyleEv')
    0 -> do
      -- TODO the awesome version of this has a toggle box so that you can choose to do horiz/vertical together (once you support separate horiz/vert left/right/top/down styles)
      -- TODO also a toggle for setting corners to common sets
      let
        mssDyn = fmap snd inputDyn
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
      captureEv1 <- singleCharacterCapture


      let
        fmapfn ss sowl = case getSEltLabelSuperStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldss -> if oldss == ss
            then Nothing
            else Just (_superOwl_id sowl, CTagSuperStyle :=> Identity (CSuperStyle (DeltaSuperStyle (oldss, ss))))
        fforfn (SuperOwlParliament selection, ss) = case Data.Maybe.mapMaybe (fmapfn ss) . toList $ selection of
          [] -> Nothing
          x  -> Just $ IM.fromList x
        outputEv = fforMaybe (attach (current selectionDyn) $ makeSuperStyleEvent tl v bl h f tr br (void $ updated focusDyn)) fforfn

        -- TODO maybe just do it when any of the cell dynamics are updated rather than when focus changes...
        -- TODO if we do it on focus change, you don't want to set when escape is pressed... so maybe it's better just to do 🖕
        setStyleEv' = makeSuperStyleEvent tl v bl h f tr br (void $ updated focusDyn)
        captureEv' = leftmost [void setStyleEv', captureEv1]
      return (4, captureEv', setStyleEv')



  setStyleEv <- switchHold never (fmap thd3 setStyleEvEv)
  captureEv <- switchHold never (fmap snd3 setStyleEvEv)
  heightDyn <- holdDyn 0 (fmap fst3 setStyleEvEv)

  let
    selectionDyn = fmap fst inputDyn
    pushSuperStyleFn :: SuperStyle -> PushM t (Maybe ControllersWithId)
    pushSuperStyleFn ss = do
      SuperOwlParliament selection <- sample . current $ selectionDyn
      let
        fmapfn sowl = case getSEltLabelSuperStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldss -> if oldss == ss
            then Nothing
            else Just (_superOwl_id sowl, CTagSuperStyle :=> Identity (CSuperStyle (DeltaSuperStyle (oldss, ss))))
      return $ case Data.Maybe.mapMaybe fmapfn . toList $ selection of
        [] -> Nothing
        x  -> Just $ IM.fromList x
    ssparamsEv = push pushSuperStyleFn setStyleEv
  return (ffor heightDyn (+1), captureEv, ssparamsEv)





-- Text Alignment stuff
holdTextAlignmentWidget :: forall t m. (MonadWidget t m) => ParamsWidgetFn t m TextAlign ControllersWithId
holdTextAlignmentWidget inputDyn = constDyn $ do
  let
    mtaDyn = fmap snd inputDyn
    selectionDyn = fmap fst inputDyn

  mta0 <- sample . current $ mtaDyn

  let
    startAlign = case mta0 of
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
        fmapfn sowl = case getSEltLabelBoxTextStyle (superOwl_toSEltLabel_hack sowl) of
          Nothing -> Nothing
          Just oldts -> if oldts == TextStyle ta
            then Nothing
            else Just (_superOwl_id sowl, CTagBoxTextStyle :=> Identity (CTextStyle (DeltaTextStyle (oldts, TextStyle ta))))
      SuperOwlParliament selection <- sample . current $ selectionDyn
      return $ case Data.Maybe.mapMaybe fmapfn . toList $ selection of
        [] -> Nothing
        x  -> Just $ IM.fromList x
    alignmentParamsEv = push pushAlignmentFn setAlignmentEv

  return (1, never, alignmentParamsEv)

holdSBoxTypeWidget :: forall t m. (MonadLayoutWidget t m) => ParamsWidgetFn t m SBoxType ControllersWithId
holdSBoxTypeWidget inputDyn = constDyn $ do
  let
    mBoxType = fmap snd inputDyn
    selectionDyn = fmap fst inputDyn
  mbt0 <- sample . current $ mBoxType
  let
    (startbox,starttext) = case mbt0 of
      Nothing                 -> ([], [])
      Just SBoxType_Box       -> ([0],[0])
      Just SBoxType_BoxText   -> ([0],[1])
      Just SBoxType_NoBox     -> ([1],[0])
      Just SBoxType_NoBoxText -> ([1],[1])

  _ <- col $ do
    b <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text "border:"
      (grout . stretch) 1 $ text "TODO CHECKBOX"
    t <- (grout . fixed) 1 $ row $ do
      (grout . fixed) 8 $ text "  text:"
      (grout . stretch) 1 $ text "TODO CHECKBOX"
    return (b,t)

  -- TODO
  return (2, never, never)

holdCanvasSizeWidget :: forall t m. (MonadLayoutWidget t m, HasPotato t m) => Dynamic t SCanvas -> ParamsWidgetFn t m () XY
holdCanvasSizeWidget canvasDyn nothingDyn = ffor nothingDyn $ \_ -> do
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
  let
    outputEv = flip push (void $ updated focusDyn) $ \_ -> do
      cw <- sample . current $ cWidthDyn
      ch <- sample . current $ cHeightDyn
      w <- sample . current $ wDyn
      h <- sample . current $ hDyn
      return $ if cw /= w || ch /= h
        then Just $ V2 (w-cw) (h-ch) -- it's a delta D:
        else Nothing
  captureEv1 <- singleCharacterCapture
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
}

data ParamsWidget t = ParamsWidget {
  -- TODO make into generic WSEvent bc we want to modify canvas as well
  _paramsWidget_paramsEvent       :: Event t ControllersWithId
  , _paramsWidget_canvasSizeEvent :: Event t XY
  , _paramsWidget_captureInputEv  :: Event t ()
}

presetStyles :: [[Char]]
presetStyles = ["╔╗╚╝║═ ","****|- ", "██████ ", "┌┐└┘│─ "]

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



holdParamsWidget :: forall t m. (MonadWidget t m, HasPotato t m)
  => ParamsWidgetConfig t
  -> m (ParamsWidget t)
holdParamsWidget ParamsWidgetConfig {..} = do
  let
    selectionDyn = _paramsWidgetConfig_selectionDyn
    canvasDyn = _paramsWidgetConfig_canvasDyn
    textAlignSelector = (fmap (\(TextStyle ta) -> ta)) . getSEltLabelBoxTextStyle . superOwl_toSEltLabel_hack
    mTextAlignInputDyn = fmap ( selectParamsFromSelection textAlignSelector) selectionDyn
    mSuperStyleInputDyn = fmap (selectParamsFromSelection (getSEltLabelSuperStyle . superOwl_toSEltLabel_hack)) selectionDyn
    --mSBoxTypeInputDyn = fmap (selectParamsFromSelection (getSEltLabelBoxType . superOwl_toSEltLabel_hack)) selectionDyn

    -- show canvas params when nothing is selected
    mCanvasSizeInputDyn = fmap (\s -> if isParliament_null s then Just (isParliament_empty, Nothing) else Nothing) selectionDyn

  (paramsOutputEv, captureEv, canvasSizeOutputEv) <- initManager_ $ do
    textAlignmentWidget <- holdMaybeParamsWidget mTextAlignInputDyn holdTextAlignmentWidget
    superStyleWidget2 <- holdMaybeParamsWidget mSuperStyleInputDyn holdSuperStyleWidget
    --sBoxTypeWidget <- holdMaybeParamsWidget mSBoxTypeInputDyn holdSBoxTypeWidget
    canvasSizeWidget <- holdMaybeParamsWidget mCanvasSizeInputDyn (holdCanvasSizeWidget canvasDyn)

    -- apparently textAlignmentWidget gets updated after any change which causes the whole network to rerender and we lose our focus state...
    let controllersWithIdParamsWidgets = fmap catMaybes . mconcat . (fmap (fmap (:[]))) $ [textAlignmentWidget, superStyleWidget2]

    (paramsOutputEv', captureEv', canvasSizeOutputEv') <- (switchHoldTriple never never never =<<) . networkView . ffor2 controllersWithIdParamsWidgets canvasSizeWidget $ \widgets mcsw -> col $ do
      outputs <- forM widgets $ \w -> mdo
        (sz, captureEv', ev) <- (tile . fixed) sz w
        return (ev, captureEv')
      -- canvas size widget is special becaues it's output type is different
      (cssev, captureEv2) <- case mcsw of
        Nothing -> return (never, never)
        Just csw -> mdo
          (cssz, csCaptureEv', cssev') <- (tile . fixed) cssz csw
          return (cssev', csCaptureEv')
      return $ (leftmostWarn "paramsLayout" (fmap fst outputs), leftmostWarn "paramsCapture" (captureEv2 : fmap snd outputs), cssev)

    return (paramsOutputEv', captureEv', canvasSizeOutputEv')

  return ParamsWidget {
    _paramsWidget_paramsEvent = paramsOutputEv
    , _paramsWidget_canvasSizeEvent = canvasSizeOutputEv
    , _paramsWidget_captureInputEv = captureEv
  }
