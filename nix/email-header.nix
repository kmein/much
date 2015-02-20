{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, containers, exceptions, fetchgit, QuickCheck
, stdenv, tasty, tasty-quickcheck, text, text-icu, time
}:
mkDerivation {
  pname = "email-header";
  version = "0.3.0";
  src = fetchgit {
    url = "https://github.com/4z3/email-header";
    sha256 = "f8e77302594d8ff163a5df75dd886f9932ef9d1a084f79c0393b48fcb2a51eb4";
    rev = "3ba16e71cf1fff92fd86199f893d89a40ca1275d";
  };
  buildDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    containers exceptions text text-icu time
  ];
  testDepends = [
    base bytestring case-insensitive containers QuickCheck tasty
    tasty-quickcheck text time
  ];
  jailbreak = true;
  homepage = "http://github.com/knrafto/email-header";
  description = "Parsing and rendering of email and MIME headers";
  license = stdenv.lib.licenses.bsd3;
}
