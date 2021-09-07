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
  -- TODO you need to do more filtering here
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
-- prob don't need this version
filenameInputFireEventOnLoseFocus
  :: (MonadWidget t m, HasPotato t m, HasFocus t m)
  => Text -- ^ initial
  -> Event t Text -- ^ override input event
  -> m (Event t Text) -- ^ event that fires when text input loses focus
filenameInputFireEventOnLoseFocus t0 overrideEv' = mdo
  dw <- displayWidth
  i <- input
  let
    overrideEv = ffor overrideEv' $ \t -> const (TZ.fromText t)
    offsetx = ffor2 dw dt $ \w fn -> max 0 (T.length fn - w + 4)
  dt <- textInputCustom' offsetx (mergeWith (.) [fmap (makeModifyEventFromUpdateTextZipperMethod updateTextZipperForFilenameCharacters) i, overrideEv]) (TZ.fromText t0)
  focusDyn <- focusedId
  lastTextDyn <- holdDyn t0 updatedtextev
  let
    updatedtextev = flip push (void $ updated focusDyn) $ \_ -> do
      t <- sample . current $ dt
      told <- sample . current $ lastTextDyn
      if t == told
        then return Nothing
        else return $ Just t
  return updatedtextev

-- UNTESTED
filenameInput
  :: (MonadWidget t m, HasPotato t m)
  => Text -- ^ initial
  -> Event t Text -- ^ override input event
  -> m (Dynamic t Text)
filenameInput t0 overrideEv' = mdo
  dw <- displayWidth
  i <- input
  let
    overrideEv = ffor overrideEv' $ \t -> const (TZ.fromText t)
    offsetx = ffor2 dw dt $ \w fn -> max 0 (T.length fn - w + 4)
  dt <- textInputCustom' offsetx (mergeWith (.) [fmap (makeModifyEventFromUpdateTextZipperMethod updateTextZipperForFilenameCharacters) i, overrideEv]) (TZ.fromText t0)
  return dt

-- | Turn a 'Span' into an 'Graphics.Vty.Image'
{-
spanToImage :: Span V.Attr -> V.Image
spanToImage (Span attrs t) = V.text' attrs t

images :: [[Span V.Attr]] -> [V.Image]
images = map (V.horizCat . map spanToImage)
-}

dropSpan :: Int -> [TZ.Span V.Attr] -> [TZ.Span V.Attr]
dropSpan _ [] = []
dropSpan n ((TZ.Span tag text):xs) = TZ.Span tag (T.drop n text) : dropSpan (max 0 (n - T.length text)) xs

renderTextZipper :: (MonadWidget t m, HasPotato t m) => Dynamic t Int -> Dynamic t Int -> Dynamic t TZ.TextZipper -> m (Dynamic t (TZ.DisplayLines V.Attr))
renderTextZipper offsetDyn dw tz = do
  f <- focus

  -- TODO do this without sampling (I think this will not update if you change style without recreating these widgets)
  -- (you could do this easily by using localTheme)
  potatostyle <- askPotato >>=  sample . _potatoConfig_style
  let
    cursorAttributes = _potatoStyle_selected potatostyle
    normalAttributes = _potatoStyle_softSelected potatostyle
    nofocusAttributes = _potatoStyle_normal potatostyle
    attrsDyn = ffor f $ \x -> if x then (normalAttributes, cursorAttributes) else (nofocusAttributes, nofocusAttributes)

  -- TODO this will still render trailing cursor when we aren't focused... please fix
  let rows = (\w s (nattr, cattr) -> TZ.displayLines w nattr cattr s)
        <$> dw
        <*> tz
        <*> attrsDyn
      img = ffor2 rows offsetDyn $ \rows' ox -> images . fmap (dropSpan ox) . TZ._displayLines_spans $ rows'
  tellImages $ (\imgs -> (:[]) . V.vertCat $ imgs) <$> current img
  return rows

-- TODO rename to singelLineTextInputCustom or something
textInputCustom'
  :: (MonadWidget t m, HasPotato t m)
  => Dynamic t Int
  -> Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> m (Dynamic t Text)
textInputCustom' offsetDyn modifyEv c0 = mdo
  rec v <- foldDyn ($) c0 $ mergeWith (.)
        [ modifyEv
        , let displayInfo = current ((,) <$> dls <*> offsetDyn)
          in ffor (attach displayInfo click) $ \((dl,ox), MouseDown _ (mx, my) _) ->
            TZ.goToDisplayLinePosition (ox+mx) my dl
        ]
      click <- mouseDown V.BLeft
      dls <- renderTextZipper offsetDyn (constDyn 999999) v
  return $ TZ.value <$> v

textInputCustom
  :: (MonadWidget t m, HasPotato t m)
  => Event t (TZ.TextZipper -> TZ.TextZipper)
  -> TZ.TextZipper
  -> m (Dynamic t Text)
textInputCustom = textInputCustom' (constDyn 0)
