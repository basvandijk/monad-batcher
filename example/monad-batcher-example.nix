{ mkDerivation, stdenv, lib
, base, monad-batcher, exceptions, dump-core
}:
mkDerivation {
  pname = "monad-batcher-example";
  version = "HEAD";
  src = lib.sourceByRegex ./. [
    "^monad-batcher-example.cabal$"
    "^example.hs$"
  ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base monad-batcher exceptions dump-core ];
  homepage = "https://github.com/basvandijk/monad-batcher";
  description = "An example for monad-batcher";
  license = stdenv.lib.licenses.bsd3;
}
