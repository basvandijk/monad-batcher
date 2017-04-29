{ mkDerivation
, stdenv, lib
, base, exceptions
}:
mkDerivation {
  pname = "monad-batcher";
  version = "HEAD";
  src = lib.sourceByRegex ./. [
    "^monad-batcher.cabal$"
    "^Setup.hs$"
    "^src$"
    "^src/.*"
    "^ChangeLog.md$"
    "^LICENSE$"
  ];
  libraryHaskellDepends = [ base exceptions ];
  homepage = "https://github.com/basvandijk/monad-batcher";
  description = "An applicative monad that batches commands for later more efficient execution";
  license = stdenv.lib.licenses.bsd3;
}
