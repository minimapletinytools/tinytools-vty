# tinytools-vty

`tinytools-vty` is a mono-space unicode diagram editor written in Haskell. It is currently a WIP and expect to have an beta release SOON.

This repository contains the vty based view/controller implementation built on top of the [tinytools](https://github.com/pdlla/tinytools) model.


# running

`cabal run tinytools-vty-exe`

you may need to install ICU depuendencies to get things to compile

# enabling unicode widechar support

## NOT WORKING, WILL CRASH RANDOMLY IF YOU USE UNICODE WIDE CHARS 😨 (this is due to bugs in TextZipper module that I still need to fix)

Unicode character display width seems to vary by terminal so you will need to generate a unicode width table file in order to enable support for unicode wide characters.
You can run `tinytools-vty` with the `--widthtable` arg to generate the table to your local config directory for the current terminal. Generating the table samples each unicode character inside the terminal and takes a few seconds to run. Please see the `Graphics.Vty.UnicodeWidthTable` module of the [vty](https://hackage.haskell.org/package/vty) for more info.
