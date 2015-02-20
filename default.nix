{ pkgs ? import <nixpkgs> {}
, src ? ./.
, name ? "much"
}:
let
  inherit (pkgs.haskellngPackages) buildLocalCabalWithArgs callPackage;
in
buildLocalCabalWithArgs {
  inherit src name;
  args = {
    email-header = callPackage ./nix/email-header.nix {};
    mime-mail = callPackage ./nix/mime-mail.nix {};
  };
}
