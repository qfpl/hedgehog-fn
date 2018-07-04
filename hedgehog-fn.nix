{ mkDerivation, base, constraints, contravariant, generics-eot
, hedgehog, stdenv
}:
mkDerivation {
  pname = "hedgehog-fn";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base constraints contravariant generics-eot hedgehog
  ];
  license = stdenv.lib.licenses.bsd3;
}
