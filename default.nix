{ mkDerivation, base, language-c, pretty, stdenv }:
mkDerivation {
  pname = "cparens";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base language-c pretty ];
  description = "Parenthesize C operators";
  license = stdenv.lib.licenses.bsd3;
}
