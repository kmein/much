{ pkgs ? import <nixpkgs> {}
, src ? ./.
, name ? "much"
}:
pkgs.haskellPackages.buildLocalCabal src name
