module Potato.Flow.Vty.Attrs (
  -- * light color scheme
  lg_default
  , lg_layer_selected
  , lg_manip
) where


import           Relude

import           Graphics.Vty

-- TODO PROBLEM this isn't used everywhere DDDDD:
lg_default :: Attr
lg_default = Attr {
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
