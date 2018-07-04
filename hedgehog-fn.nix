{ mkDerivation, base, contravariant, hedgehog, stdenv
}:
mkDerivation {
  pname = "hedgehog-fn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base contravariant hedgehog
  ];
  license = stdenv.lib.licenses.bsd3;
}
