# tinytools-vty

`tinytools-vty` is a mono-space text diagram editor written in Haskell. It is currently a WIP and expect to have an beta release SOON.

This repository contains the vty based view implementation built on top of the [tinytools](https://github.com/pdlla/tinytools) model.

# runninng

After cloning the repository, you need to run `git submodule update --init`.

After initializing all sub repositories, you can run `git pull --recurse-submodules` to pull _all_ the latest changes.

`tinytools-vty` currentyl uses the `stack` build system. Use `stack run` to run tinytools or `stack test` to run unit tests for development.


