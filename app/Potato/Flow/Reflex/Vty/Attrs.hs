module Potato.Flow.Reflex.Vty.Attrs (
  -- * light color scheme
  lg_default
  , lg_manip
) where


import           Relude

import           Graphics.Vty


lg_default :: Attr
lg_default = Attr {
  attrStyle = SetTo standout
  , attrForeColor = SetTo black
  , attrBackColor = SetTo white
  , attrURL = Default
}

lg_manip :: Attr
lg_manip = Attr {
  attrStyle = SetTo blink
  , attrForeColor = SetTo black
  , attrBackColor = SetTo brightMagenta
  , attrURL = Default
}
