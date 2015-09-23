{ mkDerivation, base, bytestring, ConfigFile, containers, directory
, filepath, Glob, hflags, lens-family-core, MissingH
, monad-parallel, mtl, pipes, pipes-bytestring, pipes-group
, pipes-safe, pipes-shell, pipes-zlib, raw-strings-qq
, regex-applicative, stdenv, time, transformers, unix
}:
mkDerivation {
  pname = "replicator";
  version = "0.5.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    base bytestring ConfigFile containers directory filepath Glob
    hflags lens-family-core MissingH monad-parallel mtl pipes
    pipes-bytestring pipes-group pipes-safe pipes-shell pipes-zlib
    raw-strings-qq regex-applicative time transformers unix
  ];
  description = "Automate creating MySQL multi-source slaves";
  license = stdenv.lib.licenses.mit;
}
