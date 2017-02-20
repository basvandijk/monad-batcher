{ mkDerivation, base, monad-batcher, stdenv }:
mkDerivation {
  pname = "monad-batcher-example";
  version = "0.0.0.0";
  sha256 = "foo";
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base monad-batcher ];
  homepage = "https://github.com/basvandijk/monad-batcher";
  description = "An example for monad-batcher";
  license = stdenv.lib.licenses.bsd3;
}
