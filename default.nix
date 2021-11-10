{ mkDerivation, base, lib, mtl }:
mkDerivation {
  pname = "delim-cont";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base mtl ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
