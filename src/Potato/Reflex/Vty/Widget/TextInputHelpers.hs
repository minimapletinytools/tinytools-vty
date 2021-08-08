-- extends methods Text.Input
-- TODO beling in Potato because depends on HasPotato
-- alternatively, drop the HasPotato requirement by passing in Behavior t V.Attr into these methods

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


updateTextZipperForSingleCharacter :: UpdateTextZipperMethod
updateTextZipperForSingleCharacter ev = case ev of
  V.EvKey (V.KChar '\t') [] -> Just $ id
  V.EvKey (V.KChar k) [] -> Just $ const $ TZ.top $ TZ.insertChar k TZ.empty
  V.EvKey V.KBS [] -> Just $ const TZ.empty
  V.EvKey V.KDel [] -> Just $ const TZ.empty
  V.EvKey (V.KChar 'u') [V.MCtrl] -> Just $ const TZ.empty
  _ -> Nothing

updateTextZipperForNumberInput
  :: UpdateTextZipperMethod
updateTextZipperForNumberInput ev = case ev of
  V.EvKey (V.KChar k) [] | isNumber k -> Just $ TZ.insertChar k
  V.EvKey V.KBS []                    -> Just $ TZ.deleteLeft
  V.EvKey V.KDel []                   -> Just $ TZ.deleteRight
  V.EvKey V.KLeft []                  -> Just $ TZ.left
  V.EvKey V.KRight []                 -> Just $ TZ.right
  V.EvKey V.KHome []                  -> Just $ TZ.home
  V.EvKey V.KEnd []                   -> Just $ TZ.end
  V.EvKey (V.KChar 'u') [V.MCtrl]     -> Just $ const TZ.empty
  _                                   -> Nothing


singleCellTextInput
  :: (MonadWidget t m, HasPotato t m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> m (Dynamic t Text)
singleCellTextInput modifyEv c0 = do
  i <- input
  textInputCustom (mergeWith (.) [fmap (makeModifyEventFromUpdateTextZipperMethod updateTextZipperForSingleCharacter) i, modifyEv]) c0


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
  tDyn <- textInputCustom (mergeWith (.) [fmap (makeModifyEventFromUpdateTextZipperMethod updateTextZipperForNumberInput) i, modifyEv]) (toText v0)
  --tDyn <- fmap _textInput_value $ textInput (def { _textInputConfig_initialValue = (toText v0)})
  return $ ffor2 valueDyn tDyn $ \v t -> fromMaybe v (readMaybe (T.unpack t))

updateTextZipperForFilenameCharacters :: UpdateTextZipperMethod
updateTextZipperForFilenameCharacters ev = case ev of
  V.EvKey (V.KChar k) [] -> Just $ TZ.insertChar k
  V.EvKey V.KBS []                    -> Just $ TZ.deleteLeft
  V.EvKey V.KDel []                   -> Just $ TZ.deleteRight
  V.EvKey V.KLeft []                  -> Just $ TZ.left
  V.EvKey V.KRight []                 -> Just $ TZ.right
  V.EvKey V.KHome []                  -> Just $ TZ.home
  V.EvKey V.KEnd []                   -> Just $ TZ.end
  V.EvKey (V.KChar 'u') [V.MCtrl]     -> Just $ const TZ.empty
  _                                   -> Nothing

-- UNTESTED
filenameInput
  :: (MonadWidget t m, HasPotato t m)
  => Event t Text -- ^ override input event
  -> m (Dynamic t Text)
filenameInput overrideEv' = do
  i <- input
  let overrideEv = ffor overrideEv' $ \t -> const (TZ.fromText t)
  textInputCustom (mergeWith (.) [fmap (makeModifyEventFromUpdateTextZipperMethod updateTextZipperForSingleCharacter) i, overrideEv]) TZ.empty

-- TODO add horiz and vert offset parameter
renderTextZipper :: (MonadWidget t m, HasPotato t m) => Dynamic t TZ.TextZipper -> m (Dynamic t (TZ.DisplayLines V.Attr))
renderTextZipper tz = do
  f <- focus
  dh <- displayHeight
  dw <- displayWidth

  -- TODO do this without sampling (I think this will not update if you change style without recreating these widgets)
  -- (you could do this easily by using localTheme)
  potatostyle <- askPotato >>=  sample . _potatoConfig_style
  let
    cursorAttributes = _potatoStyle_selected potatostyle
    normalAttributes = _potatoStyle_normal potatostyle
    -- TODO do I care about focus or no?
    cursorAttrs = ffor f $ \x -> if x then cursorAttributes else normalAttributes

  let rows = (\w s c -> TZ.displayLines w normalAttributes c s)
        <$> dw
        <*> tz
        <*> cursorAttrs
      img = images . TZ._displayLines_spans <$> rows
  tellImages $ (\imgs -> (:[]) . V.vertCat $ imgs) <$> current img
  return rows



-- TODO look into a variant that scrolls horizontally with cursor
-- TODO rename to singelLineTextInputCustom or something
textInputCustom
  :: (MonadWidget t m, HasPotato t m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> m (Dynamic t Text)
textInputCustom modifyEv c0 = mdo
  rec v <- foldDyn ($) c0 $ mergeWith (.)
        [ modifyEv
        , let displayInfo = current dls
          in ffor (attach displayInfo click) $ \(dl, MouseDown _ (mx, my) _) ->
            TZ.goToDisplayLinePosition mx my dl
        ]
      click <- mouseDown V.BLeft
      dls <- renderTextZipper v
  return $ TZ.value <$> v
