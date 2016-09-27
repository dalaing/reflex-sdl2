{ mkDerivation, base, dependent-map, dependent-sum, linear, ref-tf
, reflex, sdl2, stdenv
}:
mkDerivation {
  pname = "reflex-sdl2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base dependent-map dependent-sum linear ref-tf reflex sdl2
  ];
  license = stdenv.lib.licenses.bsd3;
}
