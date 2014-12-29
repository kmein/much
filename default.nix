{ pkgs ? import <nixpkgs> {}
, src ? ./.
, name ? "much"
}:
let
  inherit (pkgs.haskellPackages) buildLocalCabalWithArgs callPackage;
in
buildLocalCabalWithArgs {
  inherit src name;
  args = {
    friendlyTime = callPackage ./nix/friendly-time {};
  };
}
