{ mkDerivation, aeson, base, bytestring, deriving-aeson, directory
, doclayout, doctemplates, filepath, lens, lens-aeson, lib, mtl
, optparse-applicative, process, relude, temporary, text, time
, unliftio
}:
mkDerivation {
  pname = "Resume";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring deriving-aeson directory doclayout
    doctemplates filepath lens lens-aeson mtl optparse-applicative
    process relude temporary text time unliftio
  ];
  testHaskellDepends = [
    aeson base bytestring deriving-aeson directory doclayout
    doctemplates filepath lens lens-aeson mtl optparse-applicative
    process relude temporary text time unliftio
  ];
  description = "See README for more info";
  license = lib.licenses.bsd3;
}
