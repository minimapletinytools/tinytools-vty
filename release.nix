{ compiler ? "ghc948" }:

let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        # use separate name here to avoid cyclic dependencies
        overriddenHaskellPackages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = self: super: rec {
              reflex-vty = self.callHackageDirect {
                  pkg = "reflex-vty";
                  ver = "0.5.1.0";
                  sha256 = "ff25d77e771a08214f7e4e699fc31e16a6406bce51d3f35515430b882caafebd";
                } {};
              reflex-test-host = self.callHackageDirect {
                  pkg = "reflex-test-host";
                  ver = "0.1.2.3";
                  sha256 = "9ee3ad9ac4fc58c6dcabcd9fbc6d109a51d8e86ba2682cee8367bc6b452f09ea";
                } {};
              tinytools = self.callHackageDirect {
                  pkg = "tinytools";
                  ver = "0.1.0.7";
                  sha256 = "02a01546aa8d937fcde81dc814d1ee5440245b0cb0e048ccaec9714b560403f8";
                } {};
              primitive = self.callHackageDirect {
                  pkg = "primitive";
                  ver = "0.7.4.0";
                  sha256 = "02a01546aa8d937fcde81dc814d1ee5440245b0cb0e048ccaec9714b560403f8";
                } {};
              aeson-pretty = self.callHackageDirect {
                  pkg = "aeson-pretty";
                  ver = "0.8.10";
                  sha256 = "12a01546aa8d937fcde81dc814d1ee5440245b0cb0e048ccaec9714b560403f8";
                } {};
              dependent-sum-template = self.callHackageDirect {
                  pkg = "dependent-sum-template";
                  ver = "0.2.0";
                  sha256 = "22a01546aa8d937fcde81dc814d1ee5440245b0cb0e048ccaec9714b560403f8";
                } {};
              filepath = self.callHackageDirect {
                  pkg = "filepath";
                  ver = "1.4.100.0";
                  sha256 = "32a01546aa8d937fcde81dc814d1ee5440245b0cb0e048ccaec9714b560403f8";
                } {};
              mtl = self.callHackageDirect {
                  pkg = "mtl";
                  ver = "2.3.1";
                  sha256 = "42a01546aa8d937fcde81dc814d1ee5440245b0cb0e048ccaec9714b560403f8";
                } {};
            };
          };
        };
      };
    };
  };

  nixpkgs = import <nixpkgs> { inherit config; }; 
in
  { 
    tinytools-vty = nixpkgs.haskell.overriddenHaskellPackages."${compiler}".callCabal2nix "tinytools-vty" (import ./src.nix) { };
  }

