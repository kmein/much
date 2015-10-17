{
  haskell-overrides = self: super: {
    blessings = self.callPackage ./nix/blessings.nix {};
    email-header = self.callPackage ./nix/email-header.nix {};
    scanner = self.callPackage ./nix/scanner.nix {};
  };
}
