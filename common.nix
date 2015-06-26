{
  haskell-overrides = self: super: {
    email-header = self.callPackage ./nix/email-header.nix {};
  };
}
