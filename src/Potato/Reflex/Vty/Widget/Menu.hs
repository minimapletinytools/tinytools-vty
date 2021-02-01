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
import Data.Tree

type Shortcut = ()

data MenuTreeNode k = MenuTreeNode {
  _menuTreeNode_key :: k -- MUST BE UNIQUE
  , _menuTreeNode_label :: Text
  , _menuTreeNode_shortcut :: Shortcut
} deriving (Eq, Show)

data MenuWidgetConfig t k = MenuWidgetConfig {
  _menuWidgetConfig_menus :: Dynamic t (Tree (MenuTreeNode k))
}

data MenuWidget t k = MenuWidget {
  _menuWidget_selected :: Event t k
  , _menuWidget_capturedInput :: Event t ()
}

holdMenuWidget ::
  (MonadWidget t m)
  => MenuWidgetConfig t k
  -> VtyWidget t m (MenuWidget t k)
holdMenuWidget MenuWidgetConfig {..} = mdo

  -- on menu change
    -- if line down to currently opened menu still exists, keep focus (I guess to do this really right, you should track index as well so there's no menu jump glitcehs)
    -- otherwise rerender the whole darn menu and lose focus

    -- shortcutMap :: Behavior t (Map Shortcut k)

  -- focusDyn :: Dynamic t ([(k, Int, Pos)])

  -- render first layer at top of screen
    -- render each following layer




  return MenuWidget {
      _menuWidget_selected = never
      , _menuWidget_capturedInput = never
    }
