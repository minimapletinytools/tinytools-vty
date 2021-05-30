{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Widget.Menu (
  holdMenuWidget
) where

import           Relude

import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget

import qualified Graphics.Vty.Input.Events as V
import           Reflex
import           Reflex.Network
import           Reflex.Potato.Helpers
import           Reflex.Vty             


import           Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Tree

type Shortcut = ()

data MenuTreeNode k = MenuTreeNode {
  _menuTreeNode_key :: k -- MUST BE UNIQUE
  , _menuTreeNode_label :: Text
  , _menuTreeNode_shortcut :: Maybe Shortcut
} deriving (Eq, Show)

data MenuWidgetConfig t k = MenuWidgetConfig {
  _menuWidgetConfig_menus :: Dynamic t ([Tree (MenuTreeNode k)])
}

data MenuWidget t k = MenuWidget {
  _menuWidget_activated :: Event t k
  , _menuWidget_capturedInput :: Event t ()
}

indexedMerge :: (Reflex t) => [Event t a] -> Event t (Int,a)
indexedMerge evs = leftmost . fmap (\(i,ev)-> fmap (i,) ev) . zip [0..] $ evs

type Pos = (Int,Int)

type MenuFocus k = [(k, Int, Pos)]

holdMenuWidget = undefined

{-
holdMenuWidget ::
  forall t m k. (MonadWidget t m)
  => MenuWidgetConfig t k
  -> m (MenuWidget t k)
holdMenuWidget MenuWidgetConfig {..} = mdo
  let
    makeMenuWidget :: [Tree (MenuTreeNode k)] -> m ()
    makeMenuWidget roots = do
      -- TODO
      -- if line down to currently opened menu still exists, keep focus (I guess to do this really right, you should track index as well so there's no menu jump glitcehs)

      -- setup shortcut map
      let
        shortcutfmapfn MenuTreeNode {..} = case _menuTreeNode_shortcut of
          Nothing -> Nothing
          Just x -> Just (x, _menuTreeNode_key)
        rootMaybeShortcuts = fmap (fmap shortcutfmapfn) roots
        shortcuts = catMaybes . join . fmap flatten $ rootMaybeShortcuts
        shortcutMap :: Behavior t (Map.Map Shortcut k) = constant $ Map.fromList $ shortcuts

      -- render the first layer at top row
      topLayerOut <- beginLayout $ row $
        forM roots $ \(Node MenuTreeNode {..} _) -> do
          let l = T.length _menuTreeNode_label
          (tile . fixed) (constDyn $ l + 1) $ do
            -- TODO highlight if selected in focusDyn
            text (constant _menuTreeNode_label)
            click <- mouseDown V.BLeft
            return (l+1, click $> _menuTreeNode_key)
      let
        topOffsets = scanl (+) 0 $ fmap fst topLayerOut
        -- (index, xoffset)
        topEv :: Event t (Int, Int) = indexedMerge $ zipWith (\offx ev -> fmap (,offx) ev) topOffsets $ fmap snd topLayerOut

        rootSetFocusEv = ffor topEv $ \(idx,(k,xoff)) -> [(k,idx,(xoff,1))]

        childWidgetFn [] = return never
        childWidgetFn ((k,idx,(xoff,yoff)):xs) = do
          forM
          selfClickEv
          childClickEv <- childWidgetFn xs
          return $ leftmost [selfClickEv, childClickEv]

      -- TODO list to changes in focus Dyn and render each following layer
      childMenusDyn = ffor (fmap tail focusDyn) $ childWidgetFn


      focusDyn :: Dynamic t (MenuFocus k) <- undefined
      return ()


  return MenuWidget {
      _menuWidget_activated = never
      , _menuWidget_capturedInput = never
    }
-}
