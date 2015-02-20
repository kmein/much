{ nixpkgs ? import <nixpkgs> {}
, target
}:

nixpkgs.pkgs.haskellngPackages.callPackage target {}
