{ mkDerivation, async, base, bytestring, containers, directory, filepath
, mtl, stdenv, stm, text, time, hspec, QuickCheck, mwc-random, vector
, Chart, Chart-diagrams, deepseq
}:
mkDerivation {
  pname = "kmeans";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base time vector
  ];
  libraryHaskellDepends = [
    async bytestring containers directory filepath mtl stm text time
    vector mwc-random Chart Chart-diagrams deepseq
  ];
  testHaskellDepends = [
    base hspec QuickCheck
  ];
  doHaddock = false;
  description = "";
  license = stdenv.lib.licenses.mit;
}

