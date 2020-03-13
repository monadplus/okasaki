{ mkDerivation, async, base, bytestring, containers, directory, filepath
, mtl, stdenv, stm, text, time, hspec, QuickCheck, foldl
}:
mkDerivation {
  pname = "red-black-trees";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base QuickCheck foldl
  ];
  libraryHaskellDepends = [
    async bytestring containers directory filepath mtl stm text time QuickCheck
  ];
  testHaskellDepends = [
    base hspec QuickCheck
  ];
  doHaddock = false;
  description = "Red-Black Trees";
  license = stdenv.lib.licenses.mit;
}

