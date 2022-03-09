{ mkDerivation, aeson, base, bytestring, doclayout, doctemplates
, filepath, lib, mtl, optparse-applicative, relude, text, time
}:
mkDerivation {
  pname = "Resume";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring doclayout doctemplates filepath mtl
    optparse-applicative relude text time
  ];
  testHaskellDepends = [
    aeson base bytestring doclayout doctemplates filepath mtl
    optparse-applicative relude text time
  ];
  description = "See README for more info";
  license = lib.licenses.bsd3;
}
