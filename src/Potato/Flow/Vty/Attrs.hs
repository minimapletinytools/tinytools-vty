

module Potato.Flow.Vty.Attrs where


import           Relude

import Potato.Flow.Controller.Handler

import           Graphics.Vty


-- TODO PROBLEM this isn't used everywhere DDDDD:
lg_default :: Attr
lg_default = Attr {
  attrStyle = SetTo defaultStyleMask
  , attrForeColor = SetTo black
  , attrBackColor = Default
  , attrURL = Default
}

lg_layer_inheritselect :: Attr
lg_layer_inheritselect = Attr {
  attrStyle = SetTo defaultStyleMask
  , attrForeColor = SetTo black
  , attrBackColor = SetTo brightWhite
  , attrURL = Default
}

lg_layer_selected :: Attr
lg_layer_selected = Attr {
  attrStyle = SetTo standout
  , attrForeColor = SetTo black
  , attrBackColor = Default
  , attrURL = Default
}

lg_manip :: Attr
lg_manip = Attr {
  attrStyle = SetTo blink
  , attrForeColor = SetTo black
  , attrBackColor = SetTo brightMagenta
  , attrURL = Default
}

lg_canvas_cursor :: Attr
lg_canvas_cursor = Attr {
  attrStyle = SetTo blink
  , attrForeColor = SetTo black
  , attrBackColor = SetTo brightMagenta
  , attrURL = Default
}

lg_canvas_default :: Attr
lg_canvas_default = lg_default

lg_canvas_oob :: Attr
lg_canvas_oob = Attr {
  attrStyle = SetTo defaultStyleMask
  , attrForeColor = SetTo black
  , attrBackColor = SetTo brightWhite
  , attrURL = Default
}

renderHandlerColorToVtyColor :: RenderHandleColor -> Color
renderHandlerColorToVtyColor = \case
  RHC_Default -> brightMagenta
  RHC_Attachment -> brightBlue
  RHC_AttachmentHighlight -> brightCyan

lg_make_canvas_cursor :: RenderHandleColor -> Attr
lg_make_canvas_cursor rhc = Attr {
  attrStyle = SetTo blink
  , attrForeColor = SetTo black
  , attrBackColor = SetTo (renderHandlerColorToVtyColor rhc)
  , attrURL = Default
}
