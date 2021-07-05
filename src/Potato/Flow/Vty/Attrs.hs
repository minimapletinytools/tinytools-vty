

module Potato.Flow.Vty.Attrs where


import           Relude

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
