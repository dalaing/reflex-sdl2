{ mkDerivation, base, dependent-map, dependent-sum, distributive
, gl, lens, linear, mtl, primitive, ref-tf, reflex, sdl2, stdenv
, text, vector
}:
mkDerivation {
  pname = "reflex-sdl2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base dependent-map dependent-sum mtl primitive ref-tf reflex sdl2
  ];
  executableHaskellDepends = [
    base dependent-map dependent-sum distributive gl lens linear mtl
    primitive ref-tf reflex sdl2 text vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
