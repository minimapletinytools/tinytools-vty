-- extends methods Text.Input

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo     #-}

module Potato.Reflex.Vty.Widget.TextInputHelpers where

import           Relude

import           Potato.Flow
import           Potato.Flow.Vty.Attrs
import           Potato.Reflex.Vty.Helpers
import           Potato.Reflex.Vty.Widget
import Potato.Flow.Vty.PotatoReader

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


type UpdateTextZipperMethod = V.Event -> Maybe (TZ.TextZipper -> TZ.TextZipper)

makeCaptureFromUpdateTextZipperMethod :: (Reflex t, MonadFix m, MonadNodeId m, HasInput t m) => UpdateTextZipperMethod -> m (Event t())
makeCaptureFromUpdateTextZipperMethod f = do
  inp <- input
  return $ void $ fforMaybe inp f

makeModifyEventFromUpdateTextZipperMethod ::
  UpdateTextZipperMethod
  -> V.Event -- ^ The vty event to handle
  -> TZ.TextZipper -- ^ The zipper to modify
  -> TZ.TextZipper
makeModifyEventFromUpdateTextZipperMethod f = \ev -> fromMaybe id (f ev)


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
