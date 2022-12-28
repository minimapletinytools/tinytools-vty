# tinytools-vty

`tinytools-vty` is a mono-space unicode diagram editor written in Haskell. It is currently a WIP and expect to have an beta release SOON.

This repository contains the vty based view implementation built on top of the [tinytools](https://github.com/pdlla/tinytools) model.

# enabling unicode support
Unicode character display width seems to vary by terminal so you will need to generate a unicode width table file in order to enable support for unicode wide characters. 
You can run `tinytools-vty` with the `--widthtable` arg to generate the table to your local config directory for the current terminal. Generating the table samples each unicode character inside the terminal and takes a few seconds to run. Please see the `Graphics.Vty.UnicodeWidthTable` module of the [vty](https://hackage.haskell.org/package/vty) for more info.

# runninng

After cloning the repository, you need to run `git submodule update --init`.

After initializing all sub repositories, you can run `git pull --recurse-submodules` to pull _all_ the latest changes.

`tinytools-vty` currentyl uses the `stack` build system. Use `stack run` to run tinytools or `stack test` to run unit tests for development.


