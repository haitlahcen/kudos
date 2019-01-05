{ mkDerivation, base, lens, mtl, parsers, stdenv, text, trifecta
, unordered-containers
}:
mkDerivation {
  pname = "kudos";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base lens mtl parsers text trifecta unordered-containers
  ];
  executableHaskellDepends = [ base mtl text ];
  homepage = "http://github.com/haitlahcen/kudos";
  license = stdenv.lib.licenses.gpl3;
}
