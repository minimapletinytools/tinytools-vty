{-# LANGUAGE RecursiveDo          #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Vty.Test.Monad.Host (
  module Reflex.Test.Monad.Host
  , ReflexVtyTestT
  , queueVtyEvent
  , vtyInputTriggerRefs
  , userInputTriggerRefs
  , userOutputs
  , vtyOutputs
  , queueMouseEvent
  , queueMouseEventInRegion
  , queueMouseEventInRegionGated
  , queueMouseDrag
  , queueMouseDragInRegion
  , runReflexVtyTestT
  , ReflexVtyTestApp(..)
  , runReflexVtyTestApp
  -- Reflex.Vty.Widget.Test
  , splitHDrag_debug
) where

import           Relude                   hiding (getFirst)

import           Control.Monad.Ref
import qualified Data.Map                 as Map

import qualified Graphics.Vty             as V
import           Potato.Reflex.Vty.Widget
import           Reflex
import           Reflex.Host.Class
import           Reflex.Test.Monad.Host   (MonadReflexTest (..), ReflexTestT,
                                           ReflexTriggerRef,
                                           TestGuestConstraints, TestGuestT,
                                           runReflexTestT)
import           Reflex.Vty


-- for debug layout/widget stuff
import           Control.Monad.Fix
import           Data.Bimap               (Bimap)
import qualified Data.Bimap               as Bimap
import           Data.Semigroup



-- | reflex-vty variant of 'ReflexTestT' which packages an 'VtyEvent' into the input and 'Behavior t [V.Image]' into the output
-- 'uintref' and 'uout' allow user to add their own inputs and outputs
-- 'uintref' will often just be some singleton type (e.g. '()') as the app being tested still has access to the input 'Event t VtyEvent' through the 'VtyWidget' monad
type ReflexVtyTestT t uintref uout m = ReflexTestT t (uintref, ReflexTriggerRef t m VtyEvent) (uout, Behavior t [V.Image]) m

-- | queue a 'VtyEvent'
queueVtyEvent :: (MonadRef m) => VtyEvent -> ReflexVtyTestT t uintref uout m ()
queueVtyEvent vtyev = do
  (_, vtytref) <- inputTriggerRefs
  queueEventTriggerRef vtytref vtyev

-- | obtain vty inputs
vtyInputTriggerRefs :: (MonadRef m) => ReflexVtyTestT t uintref uout m (ReflexTriggerRef t m VtyEvent)
vtyInputTriggerRefs = do
  (_, vtytrefs) <- inputTriggerRefs
  return vtytrefs

-- | obtain user defined inputs
userInputTriggerRefs :: (MonadRef m) => ReflexVtyTestT t uintref uout m uintref
userInputTriggerRefs = do
  (usertrefs, _) <- inputTriggerRefs
  return usertrefs

-- | obtain user defined outputs
userOutputs :: (MonadRef m) => ReflexVtyTestT t uintref uout m uout
userOutputs = do
  (useroutputs, _) <- outputs
  return useroutputs

-- | obtain vty outputs
vtyOutputs :: (MonadRef m) => ReflexVtyTestT t uintref uout m (Behavior t [V.Image])
vtyOutputs = do
  (_, vtyoutputs) <- outputs
  return vtyoutputs

-- | queue mouse event
queueMouseEvent :: (MonadRef m)
  => Either MouseDown MouseUp -- ^ mouse coordinates are LOCAL to the input region
  -> ReflexVtyTestT t uintref uout m ()
queueMouseEvent mouse = case mouse of
  Left (MouseDown b c mods) -> queueVtyEvent $ uncurry V.EvMouseDown c b mods
  Right (MouseUp b c)       -> queueVtyEvent $ uncurry V.EvMouseUp c b


-- | queue mouse event in a 'DynRegion'
queueMouseEventInRegion :: (Reflex t, MonadSample t m, MonadRef m)
  => DynRegion t
  -> Either MouseDown MouseUp -- ^ mouse coordinates are LOCAL to the input region
  -> ReflexVtyTestT t uintref uout m ()
queueMouseEventInRegion dr mouse = do
  let
    absCoords (Region l t _ _) (x,y) = (x+l, y+t)
  region <- sample . currentRegion $ dr
  case mouse of
    Left (MouseDown b c mods) -> queueVtyEvent $ uncurry V.EvMouseDown (absCoords region c) b mods
    Right (MouseUp b c) -> queueVtyEvent $ uncurry V.EvMouseUp (absCoords region c) b

-- | queue mouse event in a 'DynRegion'
-- if (local) mouse coordinates are outside of the (absolute) region, returns False and does not queue any event
queueMouseEventInRegionGated :: (Reflex t, MonadSample t m, MonadRef m)
  => DynRegion t
  -> Either MouseDown MouseUp -- ^ mouse coordinates are LOCAL to the input region
  -> ReflexVtyTestT t uintref uout m Bool
queueMouseEventInRegionGated dr mouse = do
  region <- sample . currentRegion $ dr
  let
    absCoords (Region l t _ _) (x,y) = (x+l, y+t)
    coordinates = case mouse of
      Left (MouseDown _ c _) -> c
      Right (MouseUp _ c)    -> c
    withinRegion (Region _ _ w h) (x,y) = not $ or [ x < 0, y < 0, x >= w, y >= h ]
  if withinRegion region coordinates
    then do
      case mouse of
        Left (MouseDown b c mods) -> queueVtyEvent $ uncurry V.EvMouseDown (absCoords region c) b mods
        Right (MouseUp b c) -> queueVtyEvent $ uncurry V.EvMouseUp (absCoords region c) b
      return True
    else return False

-- | queue and fire a series of mouse events representing a mouse drag
-- returns collected outputs
queueMouseDrag :: (Reflex t, MonadSample t m, MonadRef m)
  => V.Button -- ^ button to press
  -> [V.Modifier] -- ^ modifier held during drag
  -> NonEmpty (Int,Int) -- ^ list of drag positions
  -- TODO add something like DragState to this
  -> ((Int,Int) -> ReadPhase m a) -- ^ ReadPhase to run after each normal drag
  -> ReflexVtyTestT t uintref uout m (NonEmpty [a]) -- ^ collected outputs
queueMouseDrag = queueMouseDragInRegion (DynRegion 0 0 0 0)
{-queueMouseDrag b mods ps rps = do
  let
    dragPs' = init ps
    -- if there is only 1 elt in ps, then simulate a single click
    dragPs = fromMaybe (pure (head ps)) $ viaNonEmpty id dragPs'
    endP = last ps
  initas <- forM dragPs $ \p -> do
    queueVtyEvent (uncurry V.EvMouseDown p b mods)
    fireQueuedEventsAndRead (rps p)
  queueVtyEvent (uncurry V.EvMouseUp endP (Just b))
  lastas <- fireQueuedEventsAndRead (rps endP)
  return $ initas <> (lastas :| [])
-}

-- | same as queueMouseDrag but coordinates are translated to a region
queueMouseDragInRegion :: (Reflex t, MonadSample t m, MonadRef m)
  => DynRegion t
  -> V.Button -- ^ button to press
  -> [V.Modifier] -- ^ modifier held during drag
  -> NonEmpty (Int,Int) -- ^ list of drag positions
  -- TODO add something like DragState to this
  -> ((Int,Int) -> ReadPhase m a) -- ^ ReadPhase to run after each normal drag
  -> ReflexVtyTestT t uintref uout m (NonEmpty [a]) -- ^ collected outputs
queueMouseDragInRegion region b mods ps rps = do
  let
    dragPs' = init ps
    -- if there is only 1 elt in ps, then simulate a single click
    dragPs = fromMaybe (pure (head ps)) $ viaNonEmpty id dragPs'
    endP = last ps
  initas <- forM dragPs $ \p -> do
    queueMouseEventInRegion region $ Left (MouseDown b p mods)
    fireQueuedEventsAndRead (rps p)
  queueMouseEventInRegion region $ Right (MouseUp (Just b) endP)
  lastas <- fireQueuedEventsAndRead (rps endP)
  return $ initas <> (lastas :| [])


-- | run a 'ReflexVtyTestT'
-- analogous to runReflexTestT
runReflexVtyTestT :: forall uintref uinev uout t m a.
  (MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m) -- ^ the reason for this constraint is that we need explicit access to both inner (m) and outer (TestGuestT m) monads
  => (Int, Int) -- ^ initial screen size
  -> (uinev, uintref) -- ^ make sure uintref match uinev, i.e. return values of newEventWithTriggerRef
  -> (uinev -> VtyWidget t (NodeIdT (TestGuestT t m)) uout) -- ^ VtyWidget to test
  -> ReflexVtyTestT t uintref uout m a -- ^ test monad to run
  -> m ()
runReflexVtyTestT r0 (uinput, uinputtrefs) app rtm = do

  -- generate vty events trigger
  (vinev, vintref) <- newEventWithTriggerRef

  size <- holdDyn r0 $ fforMaybe vinev $ \case
      V.EvResize w h -> Just (w, h)
      _ -> Nothing

  -- pass it on as ctx object
  let ctx = VtyWidgetCtx {
      _vtyWidgetCtx_width = fmap fst size
      , _vtyWidgetCtx_height = fmap snd size
      , _vtyWidgetCtx_input = vinev
      , _vtyWidgetCtx_focus = constDyn True
    }

  -- unwrap VtyWidget and pass to runReflexTestT
  runReflexTestT
    ((uinput, vinev), (uinputtrefs, vintref))
    (\(uinput',_) -> runNodeIdT $ runVtyWidget ctx (app uinput'))
    rtm


-- | class to help bind network and types to a 'ReflexVtyTestT'
-- analogous to ReflexTestApp
class ReflexVtyTestApp app t m | app -> t m where

  -- NOTE only use this if you really need custom input types
  -- it's a lot easier just to use the vty input events instead
  -- if you really want to use them see examples in reflex-test-host
  data VtyAppInputTriggerRefs app :: Type
  data VtyAppInputEvents app :: Type

  data VtyAppOutput app :: Type
  getApp :: VtyAppInputEvents app -> VtyWidget t (NodeIdT (TestGuestT t m)) (VtyAppOutput app)
  makeInputs :: m (VtyAppInputEvents app, VtyAppInputTriggerRefs app)

runReflexVtyTestApp :: (ReflexVtyTestApp app t m, MonadVtyApp t (TestGuestT t m), TestGuestConstraints t m)
  => (Int, Int) -- ^ initial screen size
  -> ReflexVtyTestT t (VtyAppInputTriggerRefs app) (VtyAppOutput app) m ()
  -> m ()
runReflexVtyTestApp r0 rtm = do
  inp <- makeInputs
  runReflexVtyTestT r0 inp getApp rtm


-- | creates a 'DynRegion' in absolute coordinates from a child DynRegion in the parent DynRegion coordinates
-- this method is intended for tracking the 'DynRegion' of 'VtyWidget's created through the 'pane' method
-- since the 'VtyWidgetCtx' created by 'pane' is unaware of its parent, the tracking must be handled manually by the user
absDynRegion :: (Reflex t)
  => DynRegion t -- ^ parent
  -> DynRegion t -- ^ child in parent coordinates
  -> DynRegion t -- ^ child in absolute coordinates
absDynRegion parent child = DynRegion {
    _dynRegion_left = ffor2 (_dynRegion_left parent) (_dynRegion_left child) (+)
    , _dynRegion_top = ffor2 (_dynRegion_top parent) (_dynRegion_top child) (+)
    , _dynRegion_width = _dynRegion_width child
    , _dynRegion_height = _dynRegion_height child
  }

-- Reflex.Vty.Widget.Test
integralFractionalDivide :: (Integral a, Fractional b) => a -> a -> b
integralFractionalDivide n d = fromIntegral n / fromIntegral d

-- | debug variant of splitHDrag_debug which outputs a pair of DynRegions for each pane
splitHDrag_debug :: (Reflex t, MonadFix m, MonadHold t m, MonadNodeId m)
  => Int -- ^ initial width of left panel
  -> VtyWidget t m ()
  -> VtyWidget t m a
  -> VtyWidget t m b
  -> VtyWidget t m ((a,b), (DynRegion t, DynRegion t))
splitHDrag_debug splitter0 wS wA wB = mdo
  dh <- displayHeight
  dw <- displayWidth
  w0 <- sample . current $ dw
  dragE <- drag V.BLeft
  splitterCheckpoint <- holdDyn splitter0 $ leftmost [fst <$> ffilter snd dragSplitter, resizeSplitter]
  splitterPos <- holdDyn splitter0 $ leftmost [fst <$> dragSplitter, resizeSplitter]
  splitterFrac <- holdDyn (integralFractionalDivide splitter0 w0) $ ffor (attach (current dw) (fst <$> dragSplitter)) $ \(w, x) ->
    fromIntegral x / (max 1 (fromIntegral w))
  let dragSplitter = fforMaybe (attach (current splitterCheckpoint) dragE) $
        \(splitterX, Drag (fromX, _) (toX, _) _ _ end) ->
          if splitterX == fromX then Just (toX, end) else Nothing
      regA = DynRegion 0 0 splitterPos dh
      regS = DynRegion splitterPos 0 1 dh
      regB = DynRegion (splitterPos + 1) 0 (dw - splitterPos - 1) dh
      resizeSplitter = ffor (attach (current splitterFrac) (updated dw)) $
        \(frac, w) -> round (frac * fromIntegral w)
  focA <- holdDyn True $ leftmost
    [ True <$ mA
    , False <$ mB
    ]
  (mA, rA) <- pane2 regA focA $ withMouseDown wA
  pane regS (pure False) wS
  (mB, rB) <- pane2 regB (not <$> focA) $ withMouseDown wB
  return ((rA, rB), (regA, regB))
  where
    withMouseDown x = do
      m <- mouseDown V.BLeft
      x' <- x
      return (m, x')




-- TODO DELETE I don't really remember what I did here and testing Layout seems to be more a less a mistake
-- Reflex.Vty.Widget.Layout.Test
-- | same as 'RunLayout' except returns DynRegions for each of the queries in the layout
-- NOTE this method recreates the 'DynRegion's inside each 'Tile' of the layout so is not very performant
-- a better implementation is to have Layout hold its own 'DynRegion' but I'm avoid invasive changes for now.
{-
runLayout_debug
  :: (MonadFix m, MonadHold t m, PostBuild t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Int -- ^ The positional index of the initially focused tile
  -> Event t Int -- ^ An event that shifts focus by a given number of tiles
  -> Layout t m a -- ^ The 'Layout' widget
  -> VtyWidget t m (a, Dynamic t (Map NodeId (DynRegion t)))
runLayout_debug ddir focus0 focusShift (Layout child) = mdo
  dw <- displayWidth
  dh <- displayHeight
  let main = ffor3 ddir dw dh $ \d w h -> case d of
        Orientation_Column -> h
        Orientation_Row    -> w
  pb <- getPostBuild
  ((a, focusReq), queriesEndo) <- runReaderT (runDynamicWriterT $ runEventWriterT child) $ LayoutCtx solutionMap focusDemux ddir
  let
    queries = flip appEndo [] <$> queriesEndo
    solution = ffor2 main queries $ \sz qs -> Map.fromList
      . Map.elems
      . computeEdges
      . computeSizes sz
      . fmap (fmap snd)
      . Map.fromList
      . zip [0::Integer ..]
      $ qs
    solutionMap = ffor solution $ \ss -> ffor ss $ \(offset, sz) -> LayoutSegment
      { _layoutSegment_offset = offset
      , _layoutSegment_size = sz
      }
    solutionReg = ffor2 solution ddir $ \ss dir -> ffor ss $ \(offset, sz) -> DynRegion
      { _dynRegion_top = case dir of
          Orientation_Column -> constDyn offset
          Orientation_Row    -> 0
      , _dynRegion_left = case dir of
          Orientation_Column -> 0
          Orientation_Row    -> constDyn offset
      , _dynRegion_width = case dir of
          Orientation_Column -> dw
          Orientation_Row    -> constDyn sz
      , _dynRegion_height = case dir of
          Orientation_Column -> constDyn sz
          Orientation_Row    -> dh
      }
    focusable = fmap (Bimap.fromList . zip [0..]) $
      ffor queries $ \qs -> fforMaybe qs $ \(nodeId, (f, _)) ->
        if f then Just nodeId else Nothing
    adjustFocus
      :: (Bimap Int NodeId, (Int, Maybe NodeId))
      -> Either Int NodeId
      -> (Int, Maybe NodeId)
    adjustFocus (fm, (cur, _)) (Left shift) =
      let ix = (cur + shift) `mod` (max 1 $ Bimap.size fm)
      in (ix, Bimap.lookup ix fm)
    adjustFocus (fm, (cur, _)) (Right goto) =
      let ix = fromMaybe cur $ Bimap.lookupR goto fm
      in (ix, Just goto)
    focusChange = attachWith
      adjustFocus
      (current $ (,) <$> focusable <*> focussed)
      $ leftmost [Left <$> focusShift, Left 0 <$ pb, Right . getFirst <$> focusReq]
  -- A pair (Int, Maybe NodeId) which represents the index
  -- that we're trying to focus, and the node that actually gets
  -- focused (at that index) if it exists
  focussed <- holdDyn (focus0, Nothing) focusChange
  let
    focusDemux = demux $ snd <$> focussed
  return (a, solutionReg)
-}













{-
-- class variant which I couldn't figure out how to get working...

class MonadReflexVtyTest t m | m -> t where
  type UserInputTriggerRefs m :: Type
  type UserOutputEvents m :: Type
  queueVtyEvent :: VtyEvent -> m ()

newtype ReflexVtyTestT t uintref uout m a = ReflexVtyTestT { unReflexVtyTestT :: ReflexTestT t (uintref, ReflexTriggerRef t m VtyEvent) uout m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState (AppState t m))

instance MonadTrans (ReflexVtyTestT t uintref uout) where
  lift = ReflexVtyTestT . lift

instance (r ~ ReflexTriggerRef t m VtyEvent, Monad m) => MonadReader ((uintref, r), uout) (ReflexVtyTestT t uintref uout m) where
  ask :: ReflexVtyTestT t uintref uout m ((uintref, r), uout)
  ask = ReflexVtyTestT ask --(ask :: ReflexTestT t (uintref,r) uout m (uintref,r))
--deriving instance (r ~ ReflexTriggerRef t m VtyEvent) => MonadReader (uintref,r) (ReflexVtyTestT t uintref uout m) --via ReflexTestT t (uintref,r) uout m

instance MonadReflexVtyTest t (ReflexVtyTestT t uintref uout m) where
  type UserInputTriggerRefs (ReflexVtyTestT t uintref uout m) = uintref
  type UserOutputEvents (ReflexVtyTestT t uintref uout m) = uout
  queueVtyEvent vtyev = do
    ((_, vtytref),_)  :: ((uintref, ReflexTriggerRef t m VtyEvent), uout) <- ask
    queueEventTriggerRef vtytref vtyev
-}
