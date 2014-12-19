let
  pkgs = import <nixpkgs> {};
  inherit (pkgs) callPackage;
  hsEnv = pkgs.haskellPackages.ghcWithPackagesOld (hsPkgs : with hsPkgs; [
    dataDefault
    vty

    # for NotmuchCmd
    aeson
    #blazeHtml
    caseInsensitive
    #conduit
    #conduitExtra
    process
  ]);
in
  pkgs.myEnvFun rec {
    name = "much";
    buildInputs = with pkgs; [
      hsEnv
    ];
    extraCmds = ''
      export HISTFILE=/home/tv/.history/env-${name}
      $(grep export ${hsEnv.outPath}/bin/ghc)
    '';
  }
