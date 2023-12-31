module Potato.Flow.Vty.AppKbCmd where

import           Relude

import           Potato.Reflex.Vty.Helpers

import           Reflex
import           Reflex.Vty
import qualified Graphics.Vty.Input.Events         as V

data AppKbCmd t = AppKbCmd {
  _appKbCmd_save :: Event t ()
  , _appKbCmd_open :: Event t ()
  , _appKbCmd_print :: Event t ()
  , _appKbCmd_quit :: Event t ()
  , _appKbCmd_forceQuit :: Event t ()
  , _appKbCmd_new :: Event t ()
  , _appKbCmd_debugToggleMouse :: Event t ()
  , _appKbCmd_capturedInput :: Event t ()
}

holdAppKbCmd :: (MonadWidget t m) => m (AppKbCmd t)
holdAppKbCmd = do
  inp <- input

  let
    captureKeyWithCtrl c = fforMaybe inp $ \i -> case i of
      V.EvKey (V.KChar c') [V.MCtrl] | c' == c -> Just ()
      _ -> Nothing
    saveEv = captureKeyWithCtrl 's'
    openEv = captureKeyWithCtrl 'o'
    printEv = captureKeyWithCtrl 'p'
    quitEv = captureKeyWithCtrl 'q'
    newEv = captureKeyWithCtrl 'n'

    debugToggleMouseEv = never
    --debugToggleMouseEv = captureKeyWithCtrl 'm'

    -- TODO this doesn't seem to work, prob cuz vty isn't handling shift correctly on mac
    forceQuitEv = fforMaybe inp $ \i -> case i of
      V.EvKey (V.KChar 'q') [V.MCtrl, V.MShift] -> Just ()
      V.EvKey (V.KChar 'q') [V.MShift, V.MCtrl] -> Just ()
      _ -> Nothing

    captureEv = leftmost [saveEv, openEv, quitEv, newEv, forceQuitEv]

  return $ AppKbCmd {
      _appKbCmd_save = saveEv
      , _appKbCmd_open = openEv
      , _appKbCmd_print = printEv
      , _appKbCmd_quit = quitEv
      , _appKbCmd_forceQuit = forceQuitEv
      , _appKbCmd_new = newEv
      , _appKbCmd_debugToggleMouse = debugToggleMouseEv
      , _appKbCmd_capturedInput = captureEv
    }
