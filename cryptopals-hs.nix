{ mkDerivation, base, bytestring, split, stdenv }:
mkDerivation {
  pname = "cryptopals-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring split ];
  homepage = "https://github.com/ajmccluskey/cryptopals-hs";
  description = "Solutions to cryptopals challenges";
  license = stdenv.lib.licenses.bsd3;
}
