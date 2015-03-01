{ mkDerivation, base, base64-bytestring, blaze-builder, bytestring
, fetchgit, filepath, hspec, process, random, sendmail ? "sendmail"
, stdenv, text
}:
mkDerivation {
  pname = "mime-mail";
  version = "0.4.6.2";
  src = fetchgit {
    url = "https://github.com/4z3/mime-mail";
    sha256 = "00xlibw1rdaj71y1r7qhb8ypw5prbzyz4z3rynmv9gbxrp1kz0hw";
    rev = "be4ec1958dac85bde01ae3433cb387810585c5fd";
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
