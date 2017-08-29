{ fetchFromGitHub, callPackage, mkDerivation, base, bytestring, hedgehog, split,
  stdenv, tasty , tasty-hedgehog ? {}, tasty-hunit
}:
let
  # This is kind of crappy - but given tasty-hedgehog is new, this should ensure the build works
  tasty-hedgehog-github = callPackage (fetchFromGitHub {
      owner = "qfpl";
      repo = "tasty-hedgehog";
      rev = "5da389f5534943b430300a213c5ffb5d0e13459e";
      sha256 = "04pmr9q70gakd327sywpxr7qp8jnl3b0y2sqxxxcj6zj2q45q38m";
    }) {};
  my-tasty-hedgehog =
    if tasty-hedgehog == {}
    then tasty-hedgehog-github
    else tasty-hedgehog;
in
mkDerivation {
  pname = "cryptopals-hs";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base bytestring split ];
  testHaskellDepends = [
    base bytestring hedgehog tasty my-tasty-hedgehog tasty-hunit
  ];
  homepage = "https://github.com/ajmccluskey/cryptopals-hs";
  description = "Solutions to cryptopals challenges";
  license = stdenv.lib.licenses.bsd3;
}
