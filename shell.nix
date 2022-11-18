let rp = import ./dep/reflex-platform {};
    pkgs = rp.nixpkgs;
    env = import ./env.nix;
in pkgs.mkShell {
    name = "tinytools-vty";
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
    ];
    inputsFrom = [
      (env.ghc.callCabal2nix "tinytools-vty" env.src {}).env
    ];
  }
