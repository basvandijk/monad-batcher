{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "monad-batcher";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/basvandijk/monad-batcher";
  description = "An applicative monad that batches commands for later more efficient execution";
  license = stdenv.lib.licenses.bsd3;
}
