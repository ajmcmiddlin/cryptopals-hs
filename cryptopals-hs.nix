{ mkDerivation, base, bytestring, split, stdenv, tasty, tasty-hunit
}:
mkDerivation {
  pname = "cryptopals-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring split ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  homepage = "https://github.com/ajmccluskey/cryptopals-hs";
  description = "Solutions to cryptopals challenges";
  license = stdenv.lib.licenses.bsd3;
}
