let mkPlatform = import ./dep/reflex-platform;
    thunkSource = (import ./dep/reflex-platform {}).hackGet;
    rp = mkPlatform {
      haskellOverlays = [
        (self: super: {
          reflex-potatoes = self.callCabal2nix "reflex-potatoes" (thunkSource ./dep/reflex-potatoes) {};
          tinytools = self.callCabal2nix "tinytools" (thunkSource ./dep/tinytools) {};
          reflex-test-host= self.callCabal2nix "reflex-test-host" (thunkSource ./dep/reflex-test-host) {};
        })
      ];
    };
    src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
      "release.nix"
      "default.nix"
      ".git"
      "dist"
      "dist-newstyle"
    ])) ./.;
in { inherit src; ghc = rp.ghc8_10; }
