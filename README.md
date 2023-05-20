# tinytools-vty

`tinytools-vty` is a mono-space unicode diagram editor written in Haskell

This repository contains the [reflex-vty](https://hackage.haskell.org/package/reflex-vty) based view/controller implementation built on top of the [tinytools](https://github.com/pdlla/tinytools) model.


# running

To install tiny tools run `cabal install exe:tinytools` and then run `tinytools`

Or if you are building locally then `cabal run tinytools`

# usage

When you first run `tinytools` a tutorial file will open which contains information on how to use `tinytools-vty`. This same tutorial is copied at the bottom of this README file.

`tinytools-vty` has a very intuitive interface and most operations should be clear.

hotkeys are supported, however sometimes they get captured by the OS or the terminal and never sent into the executable :(.

# enabling unicode widechar support

## NOT WORKING, WILL CRASH RANDOMLY IF YOU USE UNICODE WIDE CHARS 😨 (this is due to bugs in TextZipper module that I still need to fix)

Unicode character display width seems to vary by terminal so you will need to generate a unicode width table file in order to enable support for unicode wide characters.
You can run `tinytools-vty` with the `--widthtable` arg to generate the table to your local config directory for the current terminal. Generating the table samples each unicode character inside the terminal and takes a few seconds to run. Please see the `Graphics.Vty.UnicodeWidthTable` module of the [vty](https://hackage.haskell.org/package/vty) for more info.

# tutorial

```
             ╔══════════════════════════════╗                                                                                                                 
             ║                              ║ ╔IMPORTANT#########################╗                                                                            
   ███████████████████                      ╚>#to explore the canvas, click the  #                                                                            
   █THIS IS TINYTOOLS█═╗                      #[(p)an] button or press p         #                                                                            
   ███████████████████ ║                      #                                  #                                                                            
                       v                      #then click and drag in the canvas #                                                                            
   <═══╗   ╔══════════════════════╗           #area to move your view            #                                                                            
       ║   ║the menu on the left  ║           #                                  #                                                                            
       ╚═══║has a lot of important║═══╗       #                                  #                                                                            
           ║operations            ║   ║       ╚##################################╝                                                                            
           ╚══════════════════════╝   ║                        ^                                                                                              
                      ║               ║                        ║                                                                                              
                      v               ║                        ║                                                                                              
        ╔═══════════════════════════╗ ║                        ║                                                                                              
        ║to draw a box, click the   ║ ║                        ║                                                                                              
        ║[(b)ox] button or press b  ║ ║                        ╚════════════════════╗                                                                         
        ║                           ║ ║                                             ║                                                                         
        ║then click and drag in the ║ ║                                             ║                                                                         
        ║canvas area                ║ ║                        *------------------* ║                                                                         
        ╚═══════════════════════════╝ ║                        |this area (to the | ║                                                                         
                             ║        ╚═══════════════════════>|right) is called  |═╝                                                                         
                             ║                                 |the canvas        |                                                                           
                             v                                 |                  |                                                                           
            ╔═════════════════════════════════╗                |                  |                                                                           
            ║after creating a box, it will be ║                |                  |                                                                           
            ║selected, you can use the options║                *------------------*                                                                           
            ║in the bottom right to change its║                                                                                                               
      ╔═════║style. you can also convert your ║                                                                                                               
  <═══╝     ║box into a text box or remove its║     ╔═══════════════════════════════╗                                                                         
            ║border                           ║     ║of course there are many more  ║                                                                         
            ║                                 ║     ║features, play around!         ║                                                                         
            ║                                 ║     ║                               ║                                                                         
            ╚═════════════════════════════════╝     ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ║                               ║                                                                         
                                                    ╚═══════════════════════════════╝                                                                         

```                                                                                                                  
