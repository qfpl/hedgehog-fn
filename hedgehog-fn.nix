{ mkDerivation, base, contravariant, hedgehog, stdenv, transformers
}:
mkDerivation {
  pname = "hedgehog-fn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base contravariant hedgehog transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
