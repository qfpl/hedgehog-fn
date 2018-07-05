{ mkDerivation, base, contravariant, discrimination, hedgehog
, stdenv, transformers
}:
mkDerivation {
  pname = "hedgehog-fn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base contravariant discrimination hedgehog transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
