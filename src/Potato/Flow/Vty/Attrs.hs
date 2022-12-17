

module Potato.Flow.Vty.Attrs where


import           Relude

import Potato.Flow.Controller.Handler

import           Graphics.Vty


lg_default :: Attr
lg_default = Attr {
  attrStyle = SetTo defaultStyleMask
  , attrForeColor = SetTo black
  , attrBackColor = SetTo brightWhite
  , attrURL = Default
}

lg_textfield_normal :: Attr
lg_textfield_normal = lg_default `withBackColor` white

lg_textfield_modifying :: Attr
lg_textfield_modifying = lg_textfield_normal

lg_textfield_cursor :: Attr
lg_textfield_cursor = lg_default `withBackColor` black `withForeColor` white 


lg_layer_inheritselect :: Attr
lg_layer_inheritselect = lg_default `withBackColor` white

lg_layer_selected :: Attr
lg_layer_selected = lg_default `withStyle` standout


lg_manip :: Attr
lg_manip = lg_default `withStyle` blink `withBackColor` brightMagenta

lg_canvas_cursor :: Attr
lg_canvas_cursor = lg_default `withStyle` blink `withBackColor` brightMagenta

lg_canvas_default :: Attr
lg_canvas_default = lg_default

lg_canvas_oob :: Attr
lg_canvas_oob = lg_default `withBackColor` white

renderHandlerColorToVtyColor :: RenderHandleColor -> Color
renderHandlerColorToVtyColor = \case
  RHC_Default -> brightMagenta
  RHC_Attachment -> brightBlue
  RHC_AttachmentHighlight -> brightCyan

lg_make_canvas_cursor :: RenderHandleColor -> Attr
lg_make_canvas_cursor rhc = lg_default `withStyle` blink `withBackColor` (renderHandlerColorToVtyColor rhc)
