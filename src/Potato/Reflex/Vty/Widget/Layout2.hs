{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE UndecidableInstances       #-}


module Potato.Reflex.Vty.Widget.Layout2
  (  Orientation(..)
  , Constraint(..)
  , Layout
  , runLayout
  , TileConfig(..)
  , tile
  , fixed
  , stretch
  , col
  , row
  , dummy
  , beginLayout
  , beginLayoutD
  , tabNavigation
  , askOrientation
  ) where

import           Prelude

import qualified Relude as R

import Control.Monad.Identity (Identity(..))
import           Control.Monad.NodeId (MonadNodeId (..), NodeId)
import           Control.Monad.Reader
import Data.Functor.Misc
import           Data.Bimap           (Bimap)
import qualified Data.Bimap           as Bimap
import Data.Dependent.Map (DMap, DSum((:=>)))
import qualified Data.Dependent.Map as DMap
import           Data.Default         (Default (..))
-- TODO you only need Bin/Tip from Internal
import           Data.Map.Internal             (Map)
import qualified Data.Map.Internal as Map
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Monoid          hiding (First (..))
import           Data.Ratio           ((%))
import Data.Traversable (mapAccumL)
import           Data.Semigroup       (First (..))
import qualified Graphics.Vty         as V
import Data.Tuple.Extra

import           Reflex
import           Reflex.Host.Class    (MonadReflexCreateTrigger)
import           Reflex.Vty.Widget

import Control.Exception (assert)

-- | The main-axis orientation of a 'Layout' widget
data Orientation = Orientation_Column
                 | Orientation_Row
  deriving (Show, Read, Eq, Ord)

data LayoutSegment = LayoutSegment
  { _layoutSegment_offset :: Int
  , _layoutSegment_size   :: Int
  }

data LayoutCtx t = LayoutCtx
  { _layoutCtx_regions     :: Dynamic t (Map NodeId LayoutSegment)

  , _layoutCtx_focusDemux  :: EventSelector t (Const2 NodeId (Maybe Int))

  , _layoutCtx_orientation :: Dynamic t Orientation

  -- TODO you can delete this
  , _layoutCtx_focusAt :: Event t Int
  }


-- | The Layout monad transformer keeps track of the configuration (e.g., 'Orientation') and
-- 'Constraint's of its child widgets, apportions vty real estate to each, and acts as a
-- switchboard for focus requests. See 'tile' and 'runLayout'.
newtype Layout t m a = Layout
  { unLayout :: EventWriterT t (First (NodeId, Int))
      (DynamicWriterT t (Endo [(NodeId, (Bool, Constraint), LayoutDebugTree t, Int)])
        (ReaderT (LayoutCtx t)
          (VtyWidget t m))) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadHold t
    , MonadSample t
    , MonadFix
    , TriggerEvent t
    , PerformEvent t
    , NotReady t
    , MonadReflexCreateTrigger t
    , HasDisplaySize t
    , MonadNodeId
    , PostBuild t
    )

instance MonadTrans (Layout t) where
  lift x = Layout $ lift $ lift $ lift $ lift x

instance (Adjustable t m, MonadFix m, MonadHold t m) => Adjustable t (Layout t m) where
  runWithReplace (Layout a) e = Layout $ runWithReplace a $ fmap unLayout e
  traverseIntMapWithKeyWithAdjust f m e = Layout $ traverseIntMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjust f m e = Layout $ traverseDMapWithKeyWithAdjust (\k v -> unLayout $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = Layout $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unLayout $ f k v) m e


--------------------------------------------------------------------------------
-- Demux
--------------------------------------------------------------------------------


data DemuxKV t k v = DemuxKV { demuxKVValue :: Behavior t (Maybe (k, v)), demuxKVSelector :: EventSelector t (Const2 k (Maybe v)) }

demuxKV :: (Reflex t, Ord k) => Dynamic t (Maybe (k,v)) -> DemuxKV t k v
demuxKV d = DemuxKV (current d) (fan $ attachWith attachfn (current d) (updated d)) where
  attachfn mkv0 mkv1 = case mkv1 of
    Nothing -> case mkv0 of
      Nothing -> DMap.empty
      Just (k0,v0) -> DMap.fromList [Const2 k0 :=> Identity Nothing]
    Just (k1,v1) -> case mkv0 of
      Nothing -> DMap.fromList [Const2 k1 :=> Identity (Just v1)]
      Just (k0,v0) | k0 == k1 && v0 == v1 -> DMap.empty
      Just (k0,v0) | k0 == k1 -> DMap.fromList [Const2 k0 :=> Identity (Just v0)]
      Just (k0,v0) -> DMap.fromList [Const2 k0 :=> Identity Nothing,
                          Const2 k1 :=> Identity (Just v1)]

demuxedKV :: (Reflex t, Eq k) => DemuxKV t k v -> k -> Dynamic t (Maybe v)
demuxedKV d k = r where
  e = select (demuxKVSelector d) (Const2 k)
  fmapfn mkv = case mkv of
    Nothing -> Nothing
    Just (k',v) -> if k' == k then Just v else Nothing
  r = unsafeBuildDynamic (fmap fmapfn . sample $ demuxKVValue d) e


{-
demuxEvWithValue :: (Reflex t, Ord k) => Event t (k,v) -> EventSelector t (Const2 k (Maybe v))
demuxEvWithValue d = fan $ attachWith (\(k0,v0) (k1,v1) -> if k0 == k1
                                            then if v0 == v1
                                              then DMap.empty
                                              else DMap.fromList [Const2 k0 :=> Identity v0]
                                            else DMap.fromList [Const2 k0 :=> Identity v0,
                                                                Const2 k1 :=> Identity v1])
                              (current d) (updated d)

demuxedEvWithValue :: (Reflex t, Eq k) => EventSelector t (Const2 k (Maybe v)) -> k -> Event t (Maybe v)
demuxedEvWithValue d k = select d (Const2 k)
-}

------
differenceDynamic :: (Reflex t) => Dynamic t a -> Event t b -> Dynamic t a
differenceDynamic


findNearestFloor_ :: (Ord k) => k -> (k, a) -> (k, a) -> Map k a -> Maybe (k, a)
findNearestFloor_ target leftmost parent Map.Tip = if target < fst leftmost
  then Nothing -- error $ "Map.findNearestFloorSure: map has no element <= " <> show target
  else if target < fst parent
    then Just leftmost
    else Just parent
findNearestFloor_ target leftmost _ (Map.Bin _ k a l r) = if target == k
  then Just (k, a)
  else if target < k
    then findNearestFloor_ target leftmost (k, a) l
    else findNearestFloor_ target (k, a) (k, a) r

findNearestFloor :: (Ord k) => k -> Map k a -> Maybe (k,a)
findNearestFloor _ Map.Tip = Nothing
findNearestFloor target m@(Map.Bin _ k x _ _) = findNearestFloor_ target (k, x) (k, x) m


-- | Run a 'Layout' action
runLayoutD
  :: forall t m a. (MonadFix m, MonadHold t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Maybe Int -- ^ The positional index of the initially focused tile
  -> Event t Int -- ^ An event that shifts focus by a given number of tiles
  -> Layout t m a -- ^ The 'Layout' widget
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t Int, Int, a)
runLayoutD ddir mfocus0 focusShift (Layout child) = LayoutVtyWidget . ReaderT $ \focusReqIx -> mdo
  dw <- displayWidth
  dh <- displayHeight
  let main = ffor3 ddir dw dh $ \d w h -> case d of
        Orientation_Column -> h
        Orientation_Row    -> w
  ((a, focusReq), queriesEndo) <- runReaderT (runDynamicWriterT $ runEventWriterT child) $ LayoutCtx solutionMap focusDemux ddir undefined
  let queries = flip appEndo [] <$> queriesEndo
      solution = ffor2 main queries $ \sz qs -> Map.fromList
        . Map.elems
        . computeEdges
        . computeSizes sz
        . fmap (\(nodeid,(_,constraint),_,_) -> (nodeid,constraint))
        . Map.fromList
        . zip [0::Integer ..]
        $ qs
      solutionMap = ffor solution $ \ss -> ffor ss $ \(offset, sz) -> LayoutSegment
        { _layoutSegment_offset = offset
        , _layoutSegment_size = sz
        }


      focusableMapAccumFn acc (nodeId, (_, _), _, nKiddos) = (nextAcc, value) where
        nextAcc = acc + nKiddos
        value = (acc, nodeId)
      focusable = fmap (Bimap.fromList . snd . mapAccumL focusableMapAccumFn 0) $
        ffor queries $ \qs -> fforMaybe qs $ \n@(_, (f, _), _, _) ->
          if f then Just n else Nothing

      -- ix is focus in self index space
      -- fst of return value is child node id to focus
      -- snd of return value is focus in child's index space
      findChildFocus :: Bimap Int NodeId -> Int -> Maybe (NodeId, Int)
      findChildFocus fm ix = findNearestFloor ix (Bimap.toMap fm) >>= \(ixl, t) -> Just (t, ix-ixl)

      adjustFocus
        :: (Bimap Int NodeId)
        -> Either Int (NodeId, Int) -- left is self index, right is (child id, child index)
        -> Maybe (Int, (NodeId, Int)) -- fst is self index, snd is (child id, child index)
      adjustFocus (fm, _) (Left ix) = Just (ix, findChildFocus fm ix)
      adjustFocus (fm, _) (Right (goto, ixrel)) = do
        ix <- Bimap.lookupR goto fm
        return (ix+ixrel, (goto, ixrel))

      focusChange = attachWith adjustFocus (current focusable)
        -- TODO handle Nothing case in focusReqIx (so that event produces Nothing in this case)
        $ leftmost [Right . getFirst <$> focusReq, Left <$> (fmapMaybe id focusReqIx)]

  fm0 <- sample . current $ focusable
  totalKiddos <- sample . current $ fmap (sum . fmap (\(_,_,_,k) -> k)) queries

  -- fst is index we want to focus in self index space
  -- fst . snd is node id we want to focus
  -- snd . snd is index we want to focus in child node's index space
  focussed :: Dynamic t (Maybe (Int, (NodeId, Int))) <- holdDyn (mfocus0 >>= (\f0 -> findChildFocus fm0 f0 >>= \cf0 -> (f0, cf0))) focusChange



  data DemuxKV t k v = DemuxKV { demuxKVValue :: Behavior t (Maybe (k, v)), demuxKVSelector :: EventSelector t (Const2 k (Maybe v)) }

  demuxKV :: (Reflex t, Ord k) => Dynamic t (Maybe (k,v)) -> DemuxKV t k v
  demuxKV d = DemuxKV (current d) (fan $ attachWith attachfn (current d) (updated d)) where
    attachfn mkv0 mkv1 = case mkv1 of
      Nothing -> case mkv0 of
        Nothing -> DMap.empty
        Just (k0,v0) -> DMap.fromList [Const2 k0 :=> Identity Nothing]
      Just (k1,v1) -> case mkv0 of
        Nothing -> DMap.fromList [Const2 k1 :=> Identity (Just v1)]
        Just (k0,v0) | k0 == k1 && v0 == v1 -> DMap.empty
        Just (k0,v0) | k0 == k1 -> DMap.fromList [Const2 k0 :=> Identity (Just v0)]
        Just (k0,v0) -> DMap.fromList [Const2 k0 :=> Identity Nothing,
                            Const2 k1 :=> Identity (Just v1)]

  demuxedKV :: (Reflex t, Eq k) => DemuxKV t k v -> k -> Dynamic t (Maybe v)
  demuxedKV d k = r where
    e = select (demuxKVSelector d) (Const2 k)
    fmapfn mkv = case mkv of
      Nothing -> Nothing
      Just (k',v) -> if k' == k then Just v else Nothing
    r = unsafeBuildDynamic (fmap fmapfn . sample $ demuxKVValue d) e



  -- a focusReq comes from below, so don't propogate it back down
  let focusDemux = demuxEvWithValue $ difference (fmapMaybe snd $ updated focussed) focusReq

  R.traceShow totalKiddos $ return (emptyLayoutDebugTree, fmap fst focussed, totalKiddos, a)

-- | Run a 'Layout' action
runLayout
  :: (MonadFix m, MonadHold t m, PostBuild t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Maybe Int -- ^ The positional index of the initially focused tile
  -> Event t Int -- ^ An event that shifts focus by a given number of tiles
  -> Layout t m a -- ^ The 'Layout' widget
  -> LayoutVtyWidget t m a
runLayout ddir mfocus0 focusShift layout = fmap (\(_,_,_,a)->a) $ runLayoutD ddir mfocus0 focusShift layout

-- | Tiles are the basic building blocks of 'Layout' widgets. Each tile has a constraint
-- on its size and ability to grow and on whether it can be focused. It also allows its child
-- widget to request focus.
tile
  :: forall t b widget x m a. (Reflex t, LayoutReturn t b a, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => TileConfig t -- ^ The tile's configuration
  -> widget t m (Event t x, b) -- ^ A child widget. The 'Event' that it returns is used to request that it be focused.
  -> Layout t m a
tile (TileConfig con focusable) child = mdo
  nodeId <- getNextNodeId
  -- by calling getLayoutTree/getLayoutNumChildren here, we store the children's layout info inside the DynamicWriter
  -- runLayoutD will extract this info later
  Layout $ tellDyn $ ffor2 con focusable $ \c f -> Endo ((nodeId, (f, c), getLayoutTree @t @b @a b, getLayoutNumChildren @t @b @a b):)
  seg <- Layout $ asks $
    fmap (Map.findWithDefault (LayoutSegment 0 0) nodeId) . _layoutCtx_regions
  dw <- displayWidth
  dh <- displayHeight
  o <- askOrientation
  let cross = join $ ffor o $ \case
        Orientation_Column -> dw
        Orientation_Row -> dh
  let reg = DynRegion
        { _dynRegion_top = ffor2 seg o $ \s -> \case
            Orientation_Column -> _layoutSegment_offset s
            Orientation_Row -> 0
        , _dynRegion_left = ffor2 seg o $ \s -> \case
            Orientation_Column -> 0
            Orientation_Row -> _layoutSegment_offset s
        , _dynRegion_width = ffor3 seg cross o $ \s c -> \case
            Orientation_Column -> c
            Orientation_Row -> _layoutSegment_size s
        , _dynRegion_height = ffor3 seg cross o $ \s c -> \case
            Orientation_Column -> _layoutSegment_size s
            Orientation_Row -> c
        }
  focusChildEvSelector <- Layout $ asks _layoutCtx_focusDemux
  let focusChildEv = demuxedEvWithValue focusChildEvSelector nodeId
  focussed <- foldDyn (\mi _ -> isJust mi) False $ focusChildEv
  (focusReq, b) <- Layout $ lift $ lift $ lift $
    pane reg focussed $ runIsLayoutVtyWidget child focusChildEv
  Layout $ tellEvent $ fmap (First . swap) $ attachPromptlyDyn (getLayoutFocussedDyn @t b) (nodeId <$ focusReq)
  return $ getLayoutResult @t b


-- | Configuration options for and constraints on 'tile'
data TileConfig t = TileConfig
  { _tileConfig_constraint :: Dynamic t Constraint
    -- ^ 'Constraint' on the tile's size
  , _tileConfig_focusable  :: Dynamic t Bool
    -- ^ Whether the tile is focusable
  }


instance Reflex t => Default (TileConfig t) where
  def = TileConfig (pure $ Constraint_Min 0) (pure True)

-- | A 'tile' of a fixed size that is focusable and gains focus on click
fixed
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> widget t m a
  -> Layout t m a
fixed sz = tile (def { _tileConfig_constraint =  Constraint_Fixed <$> sz }) . clickable

-- | A 'tile' that can stretch (i.e., has no fixed size) and has a minimum size of 0.
-- This tile is focusable and gains focus on click.
stretch
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => widget t m a
  -> Layout t m a
stretch = tile def . clickable

-- | A version of 'runLayout' that arranges tiles in a column and uses 'tabNavigation' to
-- change tile focus.
col
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t Int, Int, a)
col child = do
  let
    navigateToEv = never -- TODO
  runLayoutD (pure Orientation_Column) 0 navigateToEv child

-- | A version of 'runLayout' that arranges tiles in a row and uses 'tabNavigation' to
-- change tile focus.
row
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t Int, Int, a)
row child = do
  let
    navigateToEv = never -- TODO
  runLayoutD (pure Orientation_Row) 0 navigateToEv child

-- | Use this if you need a placeholder
dummy :: (Monad m) => LayoutVtyWidget t m (LayoutDebugTree t, Int, ())
dummy = return (emptyLayoutDebugTree, 0, ())

-- | Produces an 'Event' that navigates forward one tile when the Tab key is pressed
-- and backward one tile when Shift+Tab is pressed.
tabNavigation :: (Reflex t, Monad m) => VtyWidget t m (Event t Int)
tabNavigation = do
  fwd <- fmap (const 1) <$> key (V.KChar '\t')
  back <- fmap (const (-1)) <$> key V.KBackTab
  return $ leftmost [fwd, back]

-- | Captures the click event in a 'VtyWidget' context and returns it. Useful for
-- requesting focus when using 'tile'.
clickable
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m)
  => widget t m a
  -> LayoutVtyWidget t m (Event t (), a)
clickable child = LayoutVtyWidget . ReaderT $ \focusEv -> do
  click <- mouseDown V.BLeft
  a <- runIsLayoutVtyWidget child focusEv
  return (() <$ click, a)

beginLayoutD ::
  forall m t a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t Int, Int, a)
  -> VtyWidget t m (LayoutDebugTree t, a)
beginLayoutD child = mdo
  focussed <- focus
  tabEv <- tabNavigation
  indexDyn <- foldDyn (\shift cur -> (shift + cur) `mod` totalKiddos) 0 tabEv
  let focusDyn = ffor2 focussed indexDyn $ \f index -> if f then Just index else Nothing
  (ldt, _, totalKiddos, a) <- runIsLayoutVtyWidget child (updated focusDyn)
  return (ldt, a)

-- |
beginLayout ::
  forall m t b a. (MonadHold t m, PostBuild t m, MonadFix m, MonadNodeId m)
  => LayoutVtyWidget t m (LayoutDebugTree t, Dynamic t Int, Int, a)
  -> VtyWidget t m a
beginLayout = fmap snd . beginLayoutD

-- | Retrieve the current orientation of a 'Layout'
askOrientation :: Monad m => Layout t m (Dynamic t Orientation)
askOrientation = Layout $ asks _layoutCtx_orientation

-- | Datatype representing constraints on a widget's size along the main axis (see 'Orientation')
data Constraint = Constraint_Fixed Int
                | Constraint_Min Int
  deriving (Show, Read, Eq, Ord)

-- | Compute the size of each widget "@k@" based on the total set of 'Constraint's
computeSizes
  :: Ord k
  => Int
  -> Map k (a, Constraint)
  -> Map k (a, Int)
computeSizes available constraints =
  let minTotal = sum $ ffor (Map.elems constraints) $ \case
        (_, Constraint_Fixed n) -> n
        (_, Constraint_Min n) -> n
      leftover = max 0 (available - minTotal)
      numStretch = Map.size $ Map.filter (isMin . snd) constraints
      szStretch = floor $ leftover % (max numStretch 1)
      adjustment = max 0 $ available - minTotal - szStretch * numStretch
  in snd $ Map.mapAccum (\adj (a, c) -> case c of
      Constraint_Fixed n -> (adj, (a, n))
      Constraint_Min n   -> (0, (a, n + szStretch + adj))) adjustment constraints
  where
    isMin (Constraint_Min _) = True
    isMin _                  = False

computeEdges :: (Ord k) => Map k (a, Int) -> Map k (a, (Int, Int))
computeEdges = fst . Map.foldlWithKey' (\(m, offset) k (a, sz) ->
  (Map.insert k (a, (offset, sz)) m, sz + offset)) (Map.empty, 0)








-- TODO should prob be (Branch [LayoutDebugTree] | Leaf PosDim
-- but it's weird cuz a leaf node won't know it's PosDim until combined with a Region...
data LayoutDebugTree t = LayoutDebugTree_Branch [LayoutDebugTree t] | LayoutDebugTree_Leaf

emptyLayoutDebugTree :: LayoutDebugTree t
emptyLayoutDebugTree = LayoutDebugTree_Leaf

-- TODO rename to IsLayoutReturn?
class LayoutReturn t b a where
  getLayoutResult :: b -> a

  getLayoutNumChildren :: b -> Int

  getLayoutFocussedDyn :: b -> Dynamic t Int

  getLayoutTree :: b -> LayoutDebugTree t


instance LayoutReturn t (LayoutDebugTree t, Dynamic t Int, Int, a) a where
  getLayoutResult (_,_,_,a) = a
  getLayoutNumChildren (_,_,d,_) = d
  getLayoutFocussedDyn (_,d,_,_) = d
  getLayoutTree (tree,_,_,_) = tree

instance Reflex t => LayoutReturn t a a where
  getLayoutResult = id
  getLayoutNumChildren _ = 1
  getLayoutFocussedDyn _ = constDyn 0
  getLayoutTree _ = emptyLayoutDebugTree

class IsLayoutVtyWidget l t (m :: * -> *) where
  runIsLayoutVtyWidget :: l t m a -> Event t (Maybe Int) -> VtyWidget t m a

newtype LayoutVtyWidget t m a = LayoutVtyWidget {
    unLayoutVtyWidget :: ReaderT (Event t (Maybe Int)) (VtyWidget t m) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadHold t
    , MonadSample t
    , MonadFix
    , TriggerEvent t
    , PerformEvent t
    , NotReady t
    , MonadReflexCreateTrigger t
    , HasDisplaySize t
    , MonadNodeId
    , PostBuild t
    )

instance MonadTrans (LayoutVtyWidget t) where
  lift x = LayoutVtyWidget $ lift $ lift x

instance IsLayoutVtyWidget VtyWidget t m where
  runIsLayoutVtyWidget w _ = w

instance IsLayoutVtyWidget LayoutVtyWidget t m where
  runIsLayoutVtyWidget = runReaderT . unLayoutVtyWidget
