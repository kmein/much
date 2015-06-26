{ nixpkgs ? import <nixpkgs> {}
, target
}:

let
  hspkgs = nixpkgs.pkgs.haskellngPackages.override {
    overrides = (import ./common.nix).haskell-overrides;
  };
in

hspkgs.callPackage target {}
