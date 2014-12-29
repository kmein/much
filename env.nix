{ nixpkgs ? import <nixpkgs> {} }:

let
  name = "much";
  version = "1";

  buildInputs = with pkgs; [
    hsEnv
  ];

  extraCmds = with pkgs; ''
    export MANPATH=$(ls -d $(echo "$PATH" | tr : \\n | sed -n 's:\(^/nix/store/[^/]\+\).*:\1/share/man:p') 2>/dev/null | tr \\n :)
    $(grep export ${hsEnv.outPath}/bin/ghc)
    ${mkExports staticPkgs}
  '';

  # ghcWithPackagesOld b/c terminfo collision
  hsEnv = hsPkgs.ghcWithPackagesOld (self: with self;
    terminfo.nativeBuildInputs ++
    [
      cabalInstall
      dataDefault
      vtyUi

      # for NotmuchCmd
      aeson
      #blazeHtml
      caseInsensitive
      #conduit
      #conduitExtra
      friendly-time
      process
      rosezipper
      safe
      terminalSize
    ]
  );

  hsPkgs = pkgs.haskellPackages_ghc783_profiling.override {
    extension = self: super: with self; {
      friendly-time = callPackage ./nix/friendly-time {};
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
  name = "${name}-${version}";
  inherit buildInputs extraCmds;
}
# vim: set fdm=marker :
