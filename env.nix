{ nixpkgs ? import <nixpkgs> {} }:

let
  pname = "much";
  version = "2";

  buildInputs = with pkgs; [
    hsEnv
  ];

  extraCmds = with pkgs; ''
    export MANPATH=$(ls -d $(echo "$PATH" | tr : \\n | sed -n 's:\(^/nix/store/[^/]\+\).*:\1/share/man:p') 2>/dev/null | tr \\n :)
    $(grep export ${hsEnv.outPath}/bin/ghc)
    ${mkExports staticPkgs}
  '';

  # ghcWithPackagesOld b/c terminfo collision
  hsEnv = hsPkgs.ghcWithPackages (self: with self;
    [
      aeson
      cabal-install
      case-insensitive
      docopt
      email-header
      friendly-time
      hsemail
      mbox
      mime
      mime-mail # because modified showAddress
      process
      rosezipper
      safe
      split
      terminal-size
    ]
  );

  hsPkgs = pkgs.haskellngPackages.override {
    overrides = self: super: with self; {
      email-header = callPackage ./nix/email-header.nix {};
      mime-mail = callPackage ./nix/mime-mail.nix {};
    };
  };

  pkgs = nixpkgs // staticPkgs;
  staticPkgs = with nixpkgs; {
  };

  #{{{ mkExports : set -> string
  # Create shell script that exports a set's attributes.
  mkExports = set: with builtins; with pkgs.lib.strings;
    let
      # XXX attribute names are not escaped, they have to be sane
      # XXX the value should not contain <newline>
      mkExport = k: "export ${k}=${escapeSh (getAttr k set)}";
      escapeSh = stringAsChars (c: "\\${c}");
    in
      concatStringsSep "\n" (map mkExport (attrNames set));
  #}}}

in pkgs.myEnvFun {
  name = "${pname}-${version}";
  inherit buildInputs extraCmds;
}
# vim: set fdm=marker :
