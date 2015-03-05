{ mkDerivation, base, base64-bytestring, blaze-builder, bytestring
, fetchgit, filepath, hspec, process, random, sendmail ? "sendmail"
, stdenv, text
}:
mkDerivation {
  pname = "mime-mail";
  version = "0.4.6.2";
  src = fetchgit {
    url = "https://github.com/4z3/mime-mail";
    sha256 = "fa2ecb7ca0f71513a8f4dde897ff910d94a205c4a81c6b5e107e4712438b0446";
    rev = "3d0f060fb4c58b69c72ce3b4911bff32df7329a7";
  };
  buildDepends = [
    base base64-bytestring blaze-builder bytestring filepath process
    random text
  ];
  testDepends = [ base blaze-builder bytestring hspec text ];
  configureFlags = "--ghc-option=-DMIME_MAIL_SENDMAIL_PATH=\"${sendmail}\"";
  homepage = "http://github.com/snoyberg/mime-mail";
  description = "Compose MIME email messages";
  license = stdenv.lib.licenses.mit;
}
