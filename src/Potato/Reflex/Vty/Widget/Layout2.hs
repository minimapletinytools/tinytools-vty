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
  , beginLayoutWithDebugging
  , beginLayout
  , tabNavigation
  , askOrientation
  ) where

import           Prelude

import           Control.Monad.NodeId (MonadNodeId (..), NodeId)
import           Control.Monad.Reader
import           Data.Bimap           (Bimap)
import qualified Data.Bimap           as Bimap
import           Data.Default         (Default (..))
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromMaybe)
import           Data.Monoid          hiding (First (..))
import           Data.Ratio           ((%))
import           Data.Semigroup       (First (..))
import qualified Graphics.Vty         as V
import Data.Tuple.Extra

import           Reflex
import           Reflex.Host.Class    (MonadReflexCreateTrigger)
import           Reflex.Vty.Widget

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

  -- TODO DELETE this :)
  , _layoutCtx_focusDemux  :: Demux t (Maybe NodeId)

  , _layoutCtx_orientation :: Dynamic t Orientation

  , _layoutCtx_focusAt :: Event t Int
  }


-- | The Layout monad transformer keeps track of the configuration (e.g., 'Orientation') and
-- 'Constraint's of its child widgets, apportions vty real estate to each, and acts as a
-- switchboard for focus requests. See 'tile' and 'runLayout'.
newtype Layout t m a = Layout
  { unLayout :: EventWriterT t (First NodeId)
      (DynamicWriterT t (Endo [(NodeId, (Bool, Constraint), LayoutDebugTree t)])
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

-- | Run a 'Layout' action
runLayout
  :: (MonadFix m, MonadHold t m, PostBuild t m, Monad m, MonadNodeId m)
  => Dynamic t Orientation -- ^ The main-axis 'Orientation' of this 'Layout'
  -> Int -- ^ The positional index of the initially focused tile
  -> Event t Int -- ^ An event that shifts focus by a given number of tiles
  -> Layout t m a -- ^ The 'Layout' widget
  -> LayoutVtyWidget t m a -- TODO output other stuff
runLayout ddir focus0 focusShift (Layout child) = LayoutVtyWidget . ReaderT $ \focusChildEv -> mdo
  dw <- displayWidth
  dh <- displayHeight
  pb <- getPostBuild
  let main = ffor3 ddir dw dh $ \d w h -> case d of
        Orientation_Column -> h
        Orientation_Row    -> w
  ((a, focusReq), queriesEndo) <- runReaderT (runDynamicWriterT $ runEventWriterT child) $ LayoutCtx solutionMap focusDemux ddir focusChildEv
  let queries = flip appEndo [] <$> queriesEndo
      solution = ffor2 main queries $ \sz qs -> Map.fromList
        . Map.elems
        . computeEdges
        . computeSizes sz
        . fmap (\(nodeid,(_,constraint),_) -> (nodeid,constraint))
        . Map.fromList
        . zip [0::Integer ..]
        $ qs
      solutionMap = ffor solution $ \ss -> ffor ss $ \(offset, sz) -> LayoutSegment
        { _layoutSegment_offset = offset
        , _layoutSegment_size = sz
        }
      focusable = fmap (Bimap.fromList . zip [0..]) $
        ffor queries $ \qs -> fforMaybe qs $ \(nodeId, (f, _), _) ->
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
  let focusDemux = demux $ snd <$> focussed
  return a


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
  -- TODO hope there's no infinite loop here D:
  Layout $ tellDyn $ ffor2 con focusable $ \c f -> Endo ((nodeId, (f, c), getLayoutTree @t @b @a b):)
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
  focussed <- Layout $ asks _layoutCtx_focusDemux
  (focusReq, b) <- Layout $ lift $ lift $ lift $
    pane reg (demuxed focussed $ Just nodeId) $ runIsLayoutVtyWidget child never
  Layout $ tellEvent $ First nodeId <$ focusReq
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
fixed'
  :: (Reflex t, LayoutReturn t b a, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> widget t m b
  -> Layout t m a
fixed' sz = tile (def { _tileConfig_constraint =  Constraint_Fixed <$> sz }) . clickable

-- | A 'tile' that can stretch (i.e., has no fixed size) and has a minimum size of 0.
-- This tile is focusable and gains focus on click.
stretch'
  :: (Reflex t, LayoutReturn t b a, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => widget t m b
  -> Layout t m a
stretch' = tile def . clickable

fixedD
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> widget t m (LayoutDebugTree t, Dynamic t Int, a) -- TODO you may as well change this to LayoutVtyWidget
  -> Layout t m a
fixedD = fixed'

stretchD
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => widget t m (LayoutDebugTree t, Dynamic t Int, a) -- TODO you may as well change this to LayoutVtyWidget
  -> Layout t m a
stretchD = stretch'

-- | A 'tile' of a fixed size that is focusable and gains focus on click
fixed
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => Dynamic t Int
  -> widget t m a
  -> Layout t m a
fixed = fixed'

-- | A 'tile' that can stretch (i.e., has no fixed size) and has a minimum size of 0.
-- This tile is focusable and gains focus on click.
stretch
  :: (Reflex t, IsLayoutVtyWidget widget t m, Monad m, MonadFix m, MonadNodeId m)
  => widget t m a
  -> Layout t m a
stretch = stretch'



-- | A version of 'runLayout' that arranges tiles in a column and uses 'tabNavigation' to
-- change tile focus.
col
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> LayoutVtyWidget t m a
col child = do
  let
    navigateToEv = never -- TODO
  runLayout (pure Orientation_Column) 0 navigateToEv child

-- | A version of 'runLayout' that arranges tiles in a row and uses 'tabNavigation' to
-- change tile focus.
row
  :: (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => Layout t m a
  -> LayoutVtyWidget t m a
row child = do
  let
    navigateToEv = never -- TODO
  runLayout (pure Orientation_Row) 0 navigateToEv child

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

-- |
beginLayoutWithDebugging ::
  forall m t a. (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => LayoutVtyWidget t m a
  -> VtyWidget t m (LayoutDebugTree t, a)
beginLayoutWithDebugging child = do
  tab <- tabNavigation
  a <- runIsLayoutVtyWidget child tab
  return (LayoutDebugTree, a)

-- |
beginLayout ::
  forall m t a. (MonadFix m, MonadHold t m, PostBuild t m, MonadNodeId m)
  => LayoutVtyWidget t m a
  -> VtyWidget t m a
beginLayout = fmap snd . beginLayoutWithDebugging

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









data LayoutDebugTree t = LayoutDebugTree

emptyLayoutDebugTree :: LayoutDebugTree t
emptyLayoutDebugTree = LayoutDebugTree

class LayoutReturn t l a where
  getLayoutResult :: l -> a

  getLayoutNumChildren :: l -> Dynamic t Int

  getLayoutTree :: l -> LayoutDebugTree t

instance LayoutReturn t (LayoutDebugTree t, Dynamic t Int, a) a where
  getLayoutResult (_,_,a) = a
  getLayoutNumChildren (_,d,_) = d
  getLayoutTree (tree,_,_) = tree

-- TODO DELETE
instance LayoutReturn t (Dynamic t Int, a) a where
  getLayoutResult (_,a) = a
  getLayoutNumChildren (d,_) = d
  getLayoutTree _ = emptyLayoutDebugTree

instance Reflex t => LayoutReturn t a a where
  getLayoutResult = id
  getLayoutNumChildren _ = constDyn 0
  getLayoutTree _ = emptyLayoutDebugTree

class IsLayoutVtyWidget l t (m :: * -> *) where
  runIsLayoutVtyWidget :: l t m a -> Event t Int -> VtyWidget t m a

newtype LayoutVtyWidget t m a = LayoutVtyWidget {
    unLayoutVtyWidget :: ReaderT (Event t Int) (VtyWidget t m) a
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
